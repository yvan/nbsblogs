import sys
import random
import pygame

from pygame.locals import *

DIRT, GRASS, WATER, COAL, CLOUD, WOOD = 0, 1, 2, 3, 4, 5
FIRE, SAND, GLASS, ROCK, STONE, BRICK, DIAMOND = 6, 7, 8, 9 , 10, 11, 12
# declare resources for inventory interface
resources = [DIRT, GRASS, WATER, COAL, WOOD, FIRE, SAND, GLASS, ROCK, STONE, BRICK, DIAMOND]
# white color for number in inventory
WHITE = (255,255,255)
BLACK = (0,0,0)
TILESIZE = 40
MAPWIDTH = 30
MAPHEIGHT = 20

# intialize an inventory
# empty when starting
inventory = {
    DIRT: 0,
    GRASS: 0,
    WATER: 0,
    COAL: 0,
    WOOD: 0,
    FIRE: 0,
    SAND: 0,
    GLASS: 0,
    ROCK: 0,
    STONE: 0,
    BRICK: 0,
    DIAMOND: 0
}

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

tilemap = [[DIRT for w in range(MAPWIDTH)] for h in range(MAPHEIGHT)]
for row in range(MAPHEIGHT):
    for col in range(MAPWIDTH):
        rn = random.randint(0,15)
        if rn == 0:
            tile = COAL
        elif rn in [1, 2]:
            tile = WATER
        elif rn in [3,4,5,6,7]:
            tile = GRASS
        elif rn in [7,8,9]:
            tile = WOOD
        elif rn in [9,10,11]:
            tile = ROCK
        else:
            tile = DIRT
        tilemap[row][col] = tile

pygame.init()
# add 50 pixels to the height for the inventory
DISPLAYSURF = pygame.display.set_mode((MAPWIDTH*TILESIZE,MAPHEIGHT*TILESIZE+50))
# setup a font for displaying inventory numbers
INVFONT = pygame.font.Font('freesansbold.ttf', 18)

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
            # if we press the space key
            if event.key == K_SPACE:
                # store the current tile
                currentTile = tilemap[playerPos[1]][playerPos[0]]
                # add one of that resource to the inventory
                inventory[currentTile] += 1
                # replace the tile on the ground with dirt
                tilemap[playerPos[1]][playerPos[0]] = DIRT

    # render tilemap
    for row in range(MAPHEIGHT):
        for column in range(MAPWIDTH):
            DISPLAYSURF.blit(textures[tilemap[row][column]], (column*TILESIZE,row*TILESIZE))

    # render inventory
    placePosition = 10
    for item in resources:
        DISPLAYSURF.blit(textures[item], (placePosition, MAPHEIGHT*TILESIZE+20))
        placePosition += 30
        textObj = INVFONT.render(str(inventory[item]), True, WHITE, BLACK)
        DISPLAYSURF.blit(textObj,(placePosition,MAPHEIGHT*TILESIZE+20))
        placePosition += 50

    DISPLAYSURF.blit(player,(playerPos[0]*TILESIZE, playerPos[1]*TILESIZE))
    pygame.display.update()