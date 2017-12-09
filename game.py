################################################################################
##----------------------------------------------------------------------------##
##  game.py       Chris Hinstorff and Derek Benson                            ##
##                                                                            ##
##  Contains the logic for controlling and rendering the nim/tictactoe game   ##
##                                                                            ##
##  A description of nim/tictactoe gameplay can be found in the README        ##
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
}
MESSAGES_ON_SCREEN = 6


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


################################################################################
#  Chat-related functions                                                      #
################################################################################

#
#
#
def receive_chat_default(text, author):
    receive_chat(text, author, pygame.font.SysFont("", 28))

#
#
#
def send_chat(text, font):
    global my_name
    print "Send: '%s'" % text
    if text == "start":
        start_game_with("frank")
    try:
        cast(erlPID, (Atom("clientserver"), Atom("send_message"), [unicode("tictactoe"), unicode(text), my_name]))
#    except:
#        print "something bad happened"
    finally:
        pass

    add_to_chat_log(text, True, font)

#
#
#
def receive_chat(text, author, font):
    global my_name
    print "Receive: '%s'" % text
    
    display_text = text
    from_me = True
    if author != my_name:
        display_text = "%s: %s" % (author, display_text)
        from_me = False
    add_to_chat_log(text, from_me, font)

#
#
#
def add_to_chat_log(text, outbound, font):
    render = (outbound, font.render(text, False, (240, 240, 240)))
    chat_log.append(render)


################################################################################
#  Game state related functions                                                #
################################################################################

#
#
#
def start_game_with(opponent_name):
    global op_name
    global whose_turn
    global game_started
    
    print "starting game with %s" % opponent_name
    op_name = opponent_name
    game_board = [None] * 9
    whose_turn = my_name
    game_started = True

# Game state is represented by the pair (whose_turn, game_board)
#
#
#
def load_game_state(gamestate):
    global whose_turn
    global game_board
    whose_turn, game_board = gamestate
def get_game_state():
    return (whose_turn, game_board)


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
        color = {
           None:    COLOR['nim_available'],
           my_name: COLOR['nim_zero'],
           op_name: COLOR['nim_one'],
        }[game_board[i-1]]
        draw_box(screen, nimfont, str(i), x, y, color)

#
# draw_nim draws the 3x3 box with the correct numbers and colors when playing
#          in tictactoe-mode. Also draws the four lines of the tictactoe grid.
#
# Params:
#  - screen  the pygame screen to draw on
#
def draw_ttt(screen):
    pygame.draw.rect(screen, COLOR['ttt_line'], pygame.Rect(330 + 136*1, 30, 6, 420))
    pygame.draw.rect(screen, COLOR['ttt_line'], pygame.Rect(330 + 136*2, 30, 6, 420))
    pygame.draw.rect(screen, COLOR['ttt_line'], pygame.Rect(330, 30 + 136*1, 420, 6))
    pygame.draw.rect(screen, COLOR['ttt_line'], pygame.Rect(330, 30 + 136*2, 420, 6))

    tttfont = pygame.font.SysFont("", 136)
    for i in xrange(1, 9+1):
        x, y = line_to_box(i)
        color = {
            None:    COLOR['nim_available'],
            my_name: COLOR['nim_zero'],
            op_name: COLOR['nim_one'],
        }[game_board[TTT_MAP[i]-1]]
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
        if pygame.mouse.get_pressed()[0] and box.collidepoint(pygame.mouse.get_pos()):
            print "Clicked box %s" % index
            click_box(index-1)

#
# check_click_ttt_mode detects a click then toggles between the two game modes
#
def check_click_ttt_mode():
    global ttt_mode
    global time_last_mode_switch
    rect = pygame.Rect(750, 0, 30, 30)
    if pygame.mouse.get_pressed()[0] and rect.collidepoint(pygame.mouse.get_pos()):
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
        game_board[box_index] = my_name
        whose_turn = None


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
        pygame.draw.rect(screen, COLOR['chat_box'],   pygame.Rect(0,   0, 300, 480))
        pygame.draw.rect(screen, COLOR['chat_input'], pygame.Rect(0, 440, 300,  50))

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
