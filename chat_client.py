import pygame

pygame.init()
screen = pygame.display.set_mode((300, 500))
done = False

pygame.draw.rect(screen, (128, 128, 128), pygame.Rect(0, 450, 300, 50))

pygame.font.init()
myfont = pygame.font.SysFont('Comic Sans MS', 20)
textsurface = myfont.render('Some Text', False, (255,255,255))
screen.blit(textsurface,(0,0))


while not done:
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            done = True
    pygame.display.flip()
