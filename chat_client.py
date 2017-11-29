import pygame
from text_input import TextInput

pygame.init()
screen = pygame.display.set_mode((300, 480))
done = False

MESSAGES_ON_SCREEN = 6

pygame.font.init()
myfont = pygame.font.SysFont("", 28)
textInput = TextInput(font_size = 28)
clock = pygame.time.Clock()

chat_log = []

def send_chat(text):
    print "Send: '%s'" % text
    add_to_chat_log(text, True)

def receive_chat(text):
    print "Receive: '%s'" % text
    add_to_chat_log(text, False)

def add_to_chat_log(text, outbound):
    render = (outbound, myfont.render(text, False, (240, 240, 240)))
    chat_log.append(render)
        
def blit_chat_log():
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

while not done:
    screen.fill((40, 40, 40))
    pygame.draw.rect(screen, (128, 128, 128), pygame.Rect(0, 440, 300, 50))
    blit_chat_log()

    events = pygame.event.get()
    for event in events:
        if event.type == pygame.QUIT:
            done = True

    if textInput.update(events):
        text = textInput.get_text()
        textInput.clear()
        if text != "":
            send_chat(text)
    screen.blit(textInput.get_surface(), (10, 450))

    pygame.display.update()
    clock.tick(30)
