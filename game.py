import pygame
from text_input import TextInput
import threading
from time import time as now

import erlport
from erlport.erlterms import Atom
from erlport.erlang import cast
import re

startRE = r"@(\S+)"

MESSAGES_ON_SCREEN = 6

chat_log = []
gameThread = False
ttt_mode = False
done = False
clock = None
time_last_mode_switch = 0

game_board = [None] * 9
my_name = ""
op_name = ""
whose_turn = None
game_started = False

TTT_MAP     = [None, 4,9,2,3,5,7,8,1,6]
TTT_MAP_REV = [None, 8,3,4,1,5,9,6,7,2]
COLOR = {
    "chat_box":      ( 40,  40,  40),
    "chat_input":    (128, 128, 128),
    "game_board":    (160, 200, 200),
    "ttt_line":      (100, 100, 100),
    "nim_available": (100, 100, 100),
    "nim_zero":      (150, 150, 200),
    "nim_one":       (200, 200, 150),
    "ttt_available": (100, 100, 100),
    "ttt_zero":      (150, 150, 200),
    "ttt_one":       (200, 200, 150),
}

def check_win():
    winningLines = [
        [4,9,2],
        [4,5,6],
        [4,3,8],
        [9,5,1],
        [2,7,6],
        [3,5,7],
        [8,1,6],
        [8,5,2]
    ]
    for line in winningLines:
        a = line[0]-1
        b = line[1]-1
        c = line[2]-1
        if (game_board[a] != None and game_board[a] == game_board[b]
            and game_board[b] == game_board[c]):
            return game_board[a]
    return None

def start_game_with(opponent_name):
    global op_name
    global whose_turn
    global game_started
    global game_board

    if (opponent_name == my_name):
        return

    print "starting game with %s" % opponent_name
    op_name = opponent_name
    game_board = [None] * 9
    whose_turn = op_name
    game_started = True
    args = [unicode("tictactoe"), my_name,
            unicode(opponent_name), get_game_state()]
    cast(erlPID, (Atom("tictactoegame"), Atom("request_start"), args))

erlPID = 0
def set_erlPID(pid):
    global erlPID
    erlPID = pid
    return True

def receive_chat_default(text, author):
    global my_name
    if author != my_name:
        receive_chat(text, author, pygame.font.SysFont("", 28))

def receive_game_over(sender):
    global op_name
    global game_started
    if sender == op_name:
        add_notification("Your game with {0} ended".format(op_name))
        winner = check_win()
        if winner != None:
            add_notification("{0} won!".format(winner))
        game_started = False
        op_name = ""

def receive_state(sender, state):
    global op_name
    global game_started
    if sender == op_name:
        load_game_state(state)
        winner = check_win()
        if winner != None:
            add_notification("Your game with {0} ended".format(op_name))
            add_notification("{0} won!".format(winner))
            send_game_over(op_name)
            game_started = False
            op_name = ""



def receive_start(sender, state):
    global game_started
    global op_name
    if op_name == "":
        print "{0} starting".format(my_name)
        op_name = sender
        game_started = True
        load_game_state(state)
    else:
        send_game_over(sender)

def send_chat(text, font):
    global my_name
    global op_name
    global game_started
    global done
    match = re.search(startRE, text)
    if match:
        start_game_with(match.group(1))

    if text == "!quit":
        if op_name != "":
            add_notification("Your game with {0} ended".format(op_name))
            send_game_over(op_name)
            op_name = ""
            game_started = False
        return
    elif text == "!help":
        add_notification("@{name} to challenge, !quit to quit game, !exit to exit app")
    elif text == "!exit":
        if (op_name != ""):
            send_game_over(op_name)
        done = True

    cast(erlPID, (Atom("clientserver"), Atom("send_message"), [unicode("tictactoe"), unicode(text), my_name]))

    add_to_chat_log(text, True, font)

# Game state is represented by the pair (whose_turn, game_board)
def load_game_state(gamestate):
    global whose_turn
    global game_board
    whose_turn, game_board = gamestate

def get_game_state():
    return (str(whose_turn), game_board)

def send_game_state():
    args = [unicode("tictactoe"), my_name,
            unicode(op_name), get_game_state()]
    cast(erlPID, (Atom("tictactoegame"), Atom("send_state"), args))

def send_game_over(target):
    args = [unicode("tictactoe"), my_name, unicode(target)]
    cast(erlPID, (Atom("tictactoegame"), Atom("send_over"), args))

def receive_chat(text, author, font):
    global my_name
    print "Receive: '%s'" % text

    display_text = text
    from_me = True
    if author != my_name:
        display_text = "%s: %s" % (author, display_text)
        from_me = False
    add_to_chat_log(display_text, from_me, font)

def add_to_chat_log(text, outbound, font=pygame.font.SysFont("", 28)):
    render = (outbound, font.render(text, False, (240, 240, 240)))
    chat_log.append(render)

def add_notification(text):
    add_to_chat_log("> {0}".format(text), False)

def blit_chat_log(screen):
    i = 0
    messages_remaining = min(MESSAGES_ON_SCREEN, len(chat_log))
    first = len(chat_log) - messages_remaining

    while messages_remaining > 0:
        outbound, render = chat_log[first + i]
        x = 10
        y = 440 - 40 * messages_remaining

        text_rect = render.get_rect()
        text_rect.top = 440 - 40 * messages_remaining
        if outbound:
            text_rect.right = 290
        else:
            text_rect.left = 10
        screen.blit(render, text_rect)

        i += 1
        messages_remaining -= 1

def line_to_box(i):
    return ((i-1)%3, (i-1)//3)

def draw_box(screen, font, char, x, y, color):
    xs = [395 + 142*i for i in xrange(0,4)]
    ys = [107 + 142*i for i in xrange(0,4)]
    render = font.render(char, False, color)
    text_rect = render.get_rect()
    text_rect.center = (xs[x], ys[y])
    screen.blit(render, text_rect)

def draw_nim(screen):
    nimfont = pygame.font.SysFont("", 136)

    for i in xrange(1, 9+1):
        x, y = line_to_box(i)
        name = game_board[i-1]
        if None == name:
            color = COLOR['nim_available']
        elif my_name == name:
            color = COLOR['nim_zero']
        else:
            color = COLOR['nim_one']
        draw_box(screen, nimfont, str(i), x, y, color)

def draw_ttt(screen):
    pygame.draw.rect(screen, COLOR['ttt_line'], pygame.Rect(330 + 136*1, 30, 6, 420))
    pygame.draw.rect(screen, COLOR['ttt_line'], pygame.Rect(330 + 136*2, 30, 6, 420))
    pygame.draw.rect(screen, COLOR['ttt_line'], pygame.Rect(330, 30 + 136*1, 420, 6))
    pygame.draw.rect(screen, COLOR['ttt_line'], pygame.Rect(330, 30 + 136*2, 420, 6))

    tttfont = pygame.font.SysFont("", 136)
    for i in xrange(1, 9+1):
        x, y = line_to_box(i)
        name = game_board[TTT_MAP[i]-1]
        if None == name:
            color = COLOR['nim_available']
        elif my_name == name:
            color = COLOR['nim_zero']
        else:
            color = COLOR['nim_one']
        draw_box(screen, tttfont, str(TTT_MAP[i]), x, y, color)

def click_box(index):
    global my_name
    global whose_turn
    if whose_turn == my_name:
        box_index = TTT_MAP[index+1] - 1 if ttt_mode else index
        game_board[box_index] = str(my_name)
        whose_turn = op_name
        send_game_state()

def make_click_boxes():
    xs = [336 + 142*i for i in xrange(0,4)]
    ys = [ 36 + 142*i for i in xrange(0,4)]
    boxes = []
    for i in xrange(1, 9+1):
        x,y = line_to_box(i)
        boxes.append((i, pygame.Rect(xs[x], ys[y], 136, 136)))
    return boxes

def check_click_boxes(click_boxes):
    for index, box in click_boxes:
        if pygame.mouse.get_pressed()[0] and box.collidepoint(pygame.mouse.get_pos()):
            print "Clicked box %s" % index
            click_box(index-1)

def check_click_ttt_mode():
    global ttt_mode
    global time_last_mode_switch
    rect = pygame.Rect(750, 0, 30, 30)
    if pygame.mouse.get_pressed()[0] and rect.collidepoint(pygame.mouse.get_pos()):
        if now() - time_last_mode_switch > 0.5:
            ttt_mode = not ttt_mode
            time_last_mode_switch = now()

def main():
    erlport.erlang.set_default_message_handler()
    global done
    global clock
    pygame.init()
    pygame.font.init()
    screen = pygame.display.set_mode((780, 480))
    textInput = TextInput(font_size = 28)
    myfont = pygame.font.SysFont("", 28)
    clock = pygame.time.Clock()
    click_boxes = make_click_boxes()

    done = False
    while not done:
        screen.fill(COLOR['game_board'])
        pygame.draw.rect(screen, COLOR['chat_box'],   pygame.Rect(0,   0, 300, 480))
        pygame.draw.rect(screen, COLOR['chat_input'], pygame.Rect(0, 440, 300,  50))
        blit_chat_log(screen)

        if game_started:
            if ttt_mode:
                draw_ttt(screen)
            else:
                draw_nim(screen)

            check_click_boxes(click_boxes)
            check_click_ttt_mode()

        events = pygame.event.get()
        for event in events:
            if event.type == pygame.QUIT:
                done = True

        if textInput.update(events):
            text = textInput.get_text()
            textInput.clear()
            if text != "":
                send_chat(text, myfont)
        screen.blit(textInput.get_surface(), (10, 450))

        pygame.display.update()
        clock.tick(30)
    cast(erlPID, Atom("done"))
    pygame.display.quit()
    pygame.quit()

def start_game_thread(username):
    global my_name
    global whose_turn
    my_name = unicode(username)
    whose_turn = my_name
    global gameThread
    gameThread = threading.Thread(target=main)
    gameThread.start()
    return True

def stop_game_thread():
    global done
    done = True
    gameThread.join()
    return True

if __name__ == "__main__":
    main()
