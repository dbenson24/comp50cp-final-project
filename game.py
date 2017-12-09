################################################################################
##----------------------------------------------------------------------------##
##                                                                            ##
##           game.py       Chris Hinstorff and Derek Benson                   ##
##                                                                            ##
##  Contains the logic for controlling and rendering the nim/tictactoe game   ##
##                                                                            ##
##  A description of nim/tictactoe gameplay can be found in the project       ##
##  submission PDF.                                                           ##
##                                                                            ##
##----------------------------------------------------------------------------##
################################################################################


################################################################################
#  Imports                                                                     #
################################################################################

import pygame
from text_input import TextInput
import threading
from time import time as now
import re

import erlport
from erlport.erlterms import Atom
from erlport.erlang import cast


################################################################################
#  Constants                                                                   #
################################################################################

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
    "change_mode":   (230, 230, 250),
}
MESSAGES_ON_SCREEN = 6
startRE = r"@(\S+)"


################################################################################
#  Global Variables                                                            #
################################################################################

#### Concurrency and erlang related global variables ####
gameThread = False
erlPID     = 0

#### Pygame-related global variables ####
chat_log              = []
ttt_mode              = False
done                  = False
clock                 = None
time_last_mode_switch = 0

#### Gamestate global variables ####
game_board   = [None] * 9
my_name      = ""
op_name      = ""
whose_turn   = None
game_started = False


################################################################################
#  Utility functions                                                           #
################################################################################

#
# line_to_box converts a linear index into the (x,y) indices of the following
#             box in row major order
#   1 2 3
#   4 5 6
#   7 8 9
#
# Return:
#  - (x-coord, y-coord)  the 0-indexed row,col coordinate pair
#
def line_to_box(i):
    return ((i-1)%3, (i-1)//3)

#
# mouse_clicked_in detects whether or not the mouse is currently being
#                  clicked in a specified rectangle
#
# Params:
#  - rectangle  a pygame Rect specifying a rectangle of pixels
#
# Return:
#  - whether or not the mouse was clicked there [True|False]
#
def mouse_clicked_in(rectangle):
    mouse_clicked      = pygame.mouse.get_pressed()[0]
    mouse_in_rectangle = rectangle.collidepoint(pygame.mouse.get_pos())
    return mouse_clicked and mouse_in_rectangle


################################################################################
#  Chat-related functions                                                      #
################################################################################

#
# send_chat sends a message through the server and adds it to the local chat
#           log. Also checks for typed control commands
#
# Params:
#  - text  the message to send
#  - font  the pygame font with which to render the text in the chat log
#
def send_chat(text, font):
    global my_name
    global op_name
    global game_started
    global done
    match = re.search(startRE, text)
    if match and op_name == "":
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
        return
    elif text == "!exit":
        if (op_name != ""):
            send_game_over(op_name)
        done = True
        return

    cast(erlPID, (Atom("clientserver"), Atom("send_message"), [unicode("tictactoe"), unicode(text), my_name]))

    add_to_chat_log(text, True, font)

#
# receive_chat is called by erlang when a message is received. The message
#              text is added to the chat log.
#
# NOTE: receive_chat_default is recommended for use with erlang instead
#       of this function, since you generally will not want to care about
#       how the message is rendered when calling from erlang
#
# Params:
#  - text    the message to send
#  - author  the name of the person who sent the message
#  - font    the pygame font with which to render the text in the chat log
#
def receive_chat(text, author, font):
    global my_name

    display_text = text
    from_me = True
    if author != my_name:
        display_text = "%s: %s" % (author, display_text)
        from_me = False
    add_to_chat_log(display_text, from_me, font)

#
# receive_chat_default is called by erlang when a message is received. The
#                      message text is added to the chat log
#
# Params:
#  - text    the message to send
#  - author  the name of the person who sent the message
#
def receive_chat_default(text, author):
    global my_name
    if author != my_name:
        receive_chat(text, author, pygame.font.SysFont("", 28))

#
# add_to_chat_log renders (but does not display) a chat message text. The
#                 rendering is saved and will be displayed on the screen later
#
# Params:
#  - text      the message to send
#  - outbound  whether or not the text was written by another user [True|False]
#  - font      the pygame font with which to render the text
#
def add_to_chat_log(text, outbound, font):
    render = (outbound, font.render(text, False, (240, 240, 240)))
    chat_log.append(render)

#
# add_notification adds a different kind of text to the chat box that is meant
#                  for messages to the user from the game
#
# Params:
#  - text  the text of the notification
#
def add_notification(text):
    add_to_chat_log("> {0}".format(text), False, pygame.font.SysFont("", 28))


################################################################################
#  Game state related functions                                                #
################################################################################
#
# gamestate is represented by the pair (whose_turn, game_board)
#  - whose_turn: [my_name|opponent_name]
#  - game_board: 9-element list with elements [None|my_name|opponent_name]
#
# Each element of the pair is stored as a separate global variable in python.
# It is constructed as a pair only when interfacing with erlang
#

#
# start_game_with attempts to start a game with an opponent of a specified name
#
# Params:
#  - opponent_name  the name of the desired opponent
#
def start_game_with(opponent_name):
    global op_name
    global whose_turn
    global game_started
    global game_board

    if (opponent_name == my_name or opponent_name == op_name):
        return

    op_name = opponent_name
    game_board = [None] * 9
    whose_turn = op_name
    game_started = True
    args = [unicode("tictactoe"), my_name,
            unicode(opponent_name), get_game_state()]
    cast(erlPID, (Atom("tictactoegame"), Atom("request_start"), args))

#
# receive_game_over is meant to be called from erlang when the opponent ends
#                   the game on their turn
#
# Params:
#  - sender  the username of the person who sent the game over signal
#
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

#
# receive_state is meant to be called from erlang when the opponent makes
#               a move. The local board is updated with the new game state
#
# Params:
#  - sender  the username of the person who sent the game over signal
#  - state   the updated gamestate
#
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

#
# receive_start is meant to be called from erlang when another user issues
#               a challenge to begin a game
#
# Params:
#  - sender  the user issuing the challenge
#  - state   the initial game state
#
def receive_start(sender, state):
    global game_started
    global op_name
    if op_name == "":
        op_name = sender
        game_started = True
        load_game_state(state)
    else:
        send_game_over(sender)

#
# load_game_state can be called from erlang to set the local gamestate
#                 to a specified value
#
# Params:
#  - gamestate  the desired gamestate
#
def load_game_state(gamestate):
    global whose_turn
    global game_board
    whose_turn, game_board = gamestate

#
# get_game_state can be called from erlang to get the local gamestate
#
# Return:
#  - gamestate  the gamestate
#
def get_game_state():
    return (str(whose_turn), game_board)

#
# send_game_over sends a game over signal to the opponent through erlang
#
def send_game_over(target):
    args = [unicode("tictactoe"), my_name, unicode(target)]
    cast(erlPID, (Atom("tictactoegame"), Atom("send_over"), args))

#
# send_game_state sends the local gamestate to the erlang process
#
def send_game_state():
    args = [unicode("tictactoe"), my_name,
            unicode(op_name), get_game_state()]
    cast(erlPID, (Atom("tictactoegame"), Atom("send_state"), args))


################################################################################
#  Rendering-related functions                                                 #
################################################################################

#
# blit_chat_log attaches an already-created render of the text of the most
#               recent messages to the pygame screen
#
# Params:
#  - screen  the pygame screen to draw on
#
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

#
# draw_box renders and attaches to the pygame screen the number that forms one
#          of the 9 squares of the game board
#
# Params:
#  - screen  the pygame screen to draw on
#  - font    the pygame font to render the character with
#  - char    the number or letter to be rendered as the square
#  - x       the x coordinate [0|1|2] of the 3x3 box to draw
#  - y       the y coordinate [0|1|2] of the 3x3 box to draw
#  - color   the color with which to draw the character
#
def draw_box(screen, font, char, x, y, color):
    xs = [395 + 142*i for i in xrange(0,4)]
    ys = [107 + 142*i for i in xrange(0,4)]
    render = font.render(char, False, color)
    text_rect = render.get_rect()
    text_rect.center = (xs[x], ys[y])
    screen.blit(render, text_rect)

#
# draw_nim draws the 3x3 box with the correct numbers and colors when playing
#          in nim-mode
#
# Params:
#  - screen  the pygame screen to draw on
#
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

#
# draw_nim draws the 3x3 box with the correct numbers and colors when playing
#          in tictactoe-mode. Also draws the four lines of the tictactoe grid.
#
# Params:
#  - screen  the pygame screen to draw on
#
def draw_ttt(screen):
    line_color = COLOR['ttt_line']
    pygame.draw.rect(screen, line_color, pygame.Rect(330 + 136*1, 30, 6, 420))
    pygame.draw.rect(screen, line_color, pygame.Rect(330 + 136*2, 30, 6, 420))
    pygame.draw.rect(screen, line_color, pygame.Rect(330, 30 + 136*1, 420, 6))
    pygame.draw.rect(screen, line_color, pygame.Rect(330, 30 + 136*2, 420, 6))

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


################################################################################
#  Game input related functions                                                #
################################################################################

#
# make_click_boxes generates the dimensions of the 9 game squares
#
# Return:
#  - boxes  a list of 9 click boxes
#
def make_click_boxes():
    xs = [336 + 142*i for i in xrange(0,4)]
    ys = [ 36 + 142*i for i in xrange(0,4)]
    boxes = []
    for i in xrange(1, 9+1):
        x,y = line_to_box(i)
        boxes.append((i, pygame.Rect(xs[x], ys[y], 136, 136)))
    return boxes

#
# check_click_boxes detects clicks in the 9 possible squares and calls
#                   the appropriate handling function
#
def check_click_boxes(click_boxes):
    for index, box in click_boxes:
        if mouse_clicked_in(box):
            click_box(index-1)

#
# check_click_ttt_mode detects a click then toggles between the two game modes
#
def check_click_ttt_mode():
    global ttt_mode
    global time_last_mode_switch
    rect = pygame.Rect(750, 0, 30, 30)
    if mouse_clicked_in(rect):
        if now() - time_last_mode_switch > 0.5:
            ttt_mode = not ttt_mode
            time_last_mode_switch = now()


################################################################################
#  Game logic related functions                                                #
################################################################################

#
# click_box implements the game logic for when a number is selected
#
# Params:
#  - index  the index of the box in the game_board
#
def click_box(index):
    global my_name
    global whose_turn
    if whose_turn == my_name:
        box_index = TTT_MAP[index+1] - 1 if ttt_mode else index
        game_board[box_index] = str(my_name)
        whose_turn = op_name
        send_game_state()

#
# check_win checks to see if a player has won the game
#
# Return:
#  - the name of the winner or None
#
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

################################################################################
#  Primary pygame function                                                     #
################################################################################

#
# main contains the primary logic for pygame
#
def main():
    # Global variables
    global done
    global clock

    # Initialize erlport, pygame, and game elements
    erlport.erlang.set_default_message_handler()
    pygame.init()
    pygame.font.init()
    screen = pygame.display.set_mode((780, 480))
    textInput = TextInput(font_size = 28)
    myfont = pygame.font.SysFont("", 28)
    clock = pygame.time.Clock()
    click_boxes = make_click_boxes()

    # The game loop. 1 iteration == 1 frame rendered
    done = False
    while not done:
        # draw background
        screen.fill(COLOR['game_board'])
        box_rect    = pygame.Rect(0,   0, 300, 480)
        input_rect  = pygame.Rect(0, 440, 300,  50)
        change_mode = pygame.Rect(750, 0,  30,  30)
        pygame.draw.rect(screen, COLOR['chat_box'],    box_rect)
        pygame.draw.rect(screen, COLOR['chat_input'],  input_rect)
        pygame.draw.rect(screen, COLOR['change_mode'], change_mode)
        
        # draw chat elements
        blit_chat_log(screen)

        # draw and check game elements
        if game_started:
            if ttt_mode:
                draw_ttt(screen)
            else:
                draw_nim(screen)
            check_click_boxes(click_boxes)
            check_click_ttt_mode()

        # check pygame events
        events = pygame.event.get()
        for event in events:
            if event.type == pygame.QUIT:
                done = True

        # draw and check text input events
        if textInput.update(events):
            text = textInput.get_text()
            textInput.clear()
            if text != "":
                send_chat(text, myfont)
        screen.blit(textInput.get_surface(), (10, 450))

        # finish handling frame
        pygame.display.update()
        clock.tick(30)

    cast(erlPID, Atom("done"))
    pygame.display.quit()
    pygame.quit()


################################################################################
#  Erlang-related and start/stop functions                                     #
################################################################################

#
# set_erlPID stores the erlang pid as a python global varible
#
# This function is meant to be called from erlang
#
# Params:
#  - pid  the erlang pid
#
def set_erlPID(pid):
    global erlPID
    erlPID = pid
    return True

#
# start_game_thread spawns a thread to run the game.
#
# This function is meant to be called from erlang
#
# Params:
#  - username  the username of the current player
#
def start_game_thread(username):
    global my_name
    global whose_turn
    my_name = unicode(username)
    whose_turn = my_name
    global gameThread
    gameThread = threading.Thread(target=main)
    gameThread.start()
    return True

#
# start_game_thread kills the thread running the game
#
# This function is meant to be called from erlang
#
def stop_game_thread():
    global done
    done = True
    gameThread.join()
    return True


################################################################################
#  Other                                                                       #
################################################################################

if __name__ == "__main__":
    main()
