import sys
import random
import pygame

from pygame.locals import *

# declare resources
DIRT, GRASS, WATER, COAL, CLOUD, WOOD = 0, 1, 2, 3, 4, 5
# declare valuable resources
FIRE, SAND, GLASS, ROCK, STONE, BRICK, DIAMOND = 6, 7, 8, 9 , 10, 11, 12

BLACK = (0,0,0)

TILESIZE = 40
MAPWIDTH = 30
MAPHEIGHT = 20

# import an image for each of the resources
textures = {
    DIRT: pygame.image.load('dirt.png'),
    GRASS: pygame.image.load('grass.png'),
    WATER: pygame.image.load('water.png'),
    COAL: pygame.image.load('coal.png'),
    CLOUD: pygame.image.load('cloud.png'),
    BRICK: pygame.image.load('brick.png'),
    DIAMOND: pygame.image.load('diamond.png'),
    FIRE: pygame.image.load('fire.png'),
    GLASS: pygame.image.load('glass.png'),
    ROCK: pygame.image.load('rock.png'),
    SAND: pygame.image.load('sand.png'),
    STONE: pygame.image.load('stone.png'),
    WOOD: pygame.image.load('wood.png')
}

player = pygame.image.load('char.png')
playerPos = [0,0]

# initialize the map with all dirt
tilemap = [[DIRT for w in range(MAPWIDTH)] for h in range(MAPHEIGHT)]
# for each row
for row in range(MAPHEIGHT):
    # for each column in that row
    for col in range(MAPWIDTH):
        # generate a random number
        rn = random.randint(0,15)
        # it the random number is 0
        # fill the tilef with COAL
        if rn == 0:
            tile = COAL
        # if that number is 1 or 2
        # fill the tile with water
        elif rn in [1, 2]:
            tile = WATER
        elif rn in [3,4,5,6,7]:
            tile = GRASS
        elif rn in [7,8,9]:
            tile = WOOD
        elif rn in [9,10,11]:
            tile = ROCK
        tilemap[row][col] = tile

pygame.init()
DISPLAYSURF = pygame.display.set_mode((MAPWIDTH*TILESIZE,MAPHEIGHT*TILESIZE))

while True:
    DISPLAYSURF.fill(BLACK)
    for event in pygame.event.get():
        if event.type == QUIT:
            pygame.quit()
            sys.exit()
        elif event.type == KEYDOWN:
            if event.key == K_RIGHT and playerPos[0] < MAPWIDTH - 1:
                playerPos[0] += 1
            if event.key == K_LEFT and playerPos[0] > 0:
                playerPos[0] -= 1
            if event.key == K_DOWN and playerPos[1] < MAPHEIGHT - 1:
                playerPos[1] += 1
            if event.key == K_UP and playerPos[1] > 0:
                playerPos[1] -= 1

    # render the tilemap we generated above
    # make sure to render this before the character
    # if not the character may not appear (hell be under the map)
    # the rednering order matters
    for row in range(MAPHEIGHT):
        for column in range(MAPWIDTH):
            DISPLAYSURF.blit(textures[tilemap[row][column]], (column*TILESIZE,row*TILESIZE))

    DISPLAYSURF.blit(player,(playerPos[0]*TILESIZE, playerPos[1]*TILESIZE))
    pygame.display.update()