import pygame
from text_input import TextInput
import threading

import erlport
from erlport.erlterms import Atom
from erlport.erlang import cast


MESSAGES_ON_SCREEN = 6

chat_log = []
gameThread = False
done = False
erlPID = 0

def set_erlPID(pid):
    global erlPID
    erlPID = pid
    return True

def receive_chat_default(text):
    receive_chat(text, pygame.font.SysFont("", 28))

def send_chat(text, font):
    print "Send: '%s'" % text
    try:
        #call()
        cast(erlPID, (Atom("clientserver"), Atom("send_message"), [str("tictactoe"), str(text)]))
    except:
        print "something bad happened"



    add_to_chat_log(text, True, font)

def receive_chat(text, font):
    print "Receive: '%s'" % text
    add_to_chat_log(text, False, font)

def add_to_chat_log(text, outbound, font):
    render = (outbound, font.render(text, False, (240, 240, 240)))
    chat_log.append(render)

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

def throwAwayMessages(m):
    return

def main():
    erlport.erlang.set_default_message_handler()
    global done
    pygame.init()
    pygame.font.init()
    screen = pygame.display.set_mode((300, 480))
    textInput = TextInput(font_size = 28)
    myfont = pygame.font.SysFont("", 28)
    clock = pygame.time.Clock()

    done = False
    while not done:
        screen.fill((40, 40, 40))
        pygame.draw.rect(screen, (128, 128, 128), pygame.Rect(0, 440, 300, 50))
        blit_chat_log(screen)

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
    cast(erlPID, Atom(b'done'))
    pygame.display.quit()
    pygame.quit()

def start_game_thread():
    global gameThread
    erlport.erlang.set_message_handler(throwAwayMessages)
    gameThread = threading.Thread(target=main)
    gameThread.start()
    return True

def stop_game_thread():
    global done
    done = True
    gameThread.join()
    return True

