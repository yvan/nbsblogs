import sys
import pygame

# this imports things like KEYDOWN, QUIT, and other
# useful pygame constants so we have them readily available
from pygame.locals import *

# create a black color
# R,G,B = 0, 0, 0
BLACK = (0,0,0)

TILESIZE = 40
MAPWIDTH = 30
MAPHEIGHT = 20

player = pygame.image.load('char.png')
playerPos = [0,0]

pygame.init()
DISPLAYSURF = pygame.display.set_mode((MAPWIDTH*TILESIZE,MAPHEIGHT*TILESIZE))

# this loop runs once every few miliseconds
# executing the code inside it
while True:
    # fill the whole screen with black again
    # to get rid of the old character image
    DISPLAYSURF.fill(BLACK)
    # grab events that pygame throws at us
    # pygame watches our game screen and
    # sends events to this loop inside
    # pygame.event.get() every loop iteration
    for event in pygame.event.get():
        # if the player presses the
        # close button, quit the game
        if event.type == QUIT:
            pygame.quit()
            sys.exit()
        # when a user presses a key a KEYDOWN
        # event will fire, the next loop iteration
        elif event.type == KEYDOWN:
            # to move the user right playerPos[0] controls left/right
            if event.key == K_RIGHT and playerPos[0] < MAPWIDTH - 1:
                playerPos[0] += 1
            # move left
            if event.key == K_LEFT and playerPos[0] > 0:
                playerPos[0] -= 1
            # move down playrePos[1] controls up/down, notice that
            # to move down we increase the position value
            if event.key == K_DOWN and playerPos[1] < MAPHEIGHT - 1:
                playerPos[1] += 1
            # move up, to move up we decrease the position value
            if event.key == K_UP and playerPos[1] > 0:
                playerPos[1] -= 1

    # re-render the players character at the new playerPos
    DISPLAYSURF.blit(player,(playerPos[0]*TILESIZE, playerPos[1]*TILESIZE))
    pygame.display.update()