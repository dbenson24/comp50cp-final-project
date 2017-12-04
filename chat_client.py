import pygame
from text_input import TextInput
import threading

MESSAGES_ON_SCREEN = 6

chat_log = []
gameThread = False
ttt_mode = False
done = False
gamestate] * 9


COLOR = {
    "chat_box":      ( 40,  40,  40),
    "chat_input":    (128, 128, 128),
    "game_board":    (160, 200, 200),
    "ttt_line":      (100, 100, 100),
    "nim_available": (100, 100, 100), 
}

def receive_chat_default(text):
    receive_chat(text, pygame.font.SysFont("", 28))

def send_chat(text, font):
    print "Send: '%s'" % text
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
        draw_box(screen, nimfont, str(i), x, y, COLOR['nim_available'])

def draw_ttt(screen):
    pygame.draw.rect(screen, COLOR['ttt_line'], pygame.Rect(330 + 136*1, 30, 6, 420))
    pygame.draw.rect(screen, COLOR['ttt_line'], pygame.Rect(330 + 136*2, 30, 6, 420))
    pygame.draw.rect(screen, COLOR['ttt_line'], pygame.Rect(330, 30 + 136*1, 420, 6))
    pygame.draw.rect(screen, COLOR['ttt_line'], pygame.Rect(330, 30 + 136*2, 420, 6))

    tttfont = pygame.font.SysFont("", 136)
    cells = [None, 4,9,2,3,5,7,8,1,6]
    for i in xrange(1, 9+1):
        x, y = line_to_box(i)
        draw_box(screen, tttfont, str(cells[i]), x, y, COLOR['nim_available'])

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
            #click_box(index)
            print "Clicked box %s" % index

def main():
    global done
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

        if ttt_mode:
            draw_ttt(screen)
        else:
            draw_nim(screen)

        check_click_boxes(click_boxes)
            
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
    pygame.display.quit()
    pygame.quit()

def start_game_thread():
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
