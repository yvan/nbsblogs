import sys
import random
import pygame

from pygame.locals import *

DIRT, GRASS, WATER, COAL, CLOUD, WOOD = 0, 1, 2, 3, 4, 5
FIRE, SAND, GLASS, ROCK, STONE, BRICK, DIAMOND = 6, 7, 8, 9 , 10, 11, 12
resources = [DIRT, GRASS, WATER, COAL, WOOD, FIRE, SAND, GLASS, ROCK, STONE, BRICK, DIAMOND]
WHITE = (255,255,255)
BLACK = (0,0,0)
TILESIZE = 40
MAPWIDTH = 30
MAPHEIGHT = 20

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

# add controls for crafting
# these numbers correspond to
# number keys 1,2,...0, -, =
controls = {
    DIRT: 49, # 1
    GRASS: 50, # 2
    WATER: 51, # 3
    COAL: 52, # 4
    WOOD: 53, # 5
    FIRE: 54, # 6
    SAND: 55, # 7
    GLASS: 56, # 8
    ROCK: 57, # 9 
    STONE: 48, # 0
    BRICK: 45, # -
    DIAMOND: 61 # =
}

# this tells us how many or each resource
# we need to produce a more valuable resource
# so 1 stone takes 1 rock
craft = {
    STONE: {ROCK: 1},
    SAND: {ROCK: 1},
    FIRE: {WOOD: 1},
    GLASS: {FIRE: 1, SAND:1},
    DIAMOND: {WOOD:1, COAL:1},
    BRICK: {ROCK:1, FIRE:1}
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
DISPLAYSURF = pygame.display.set_mode((MAPWIDTH*TILESIZE,MAPHEIGHT*TILESIZE+50))
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
            if event.key == K_SPACE:
                currentTile = tilemap[playerPos[1]][playerPos[0]]
                inventory[currentTile] += 1
                tilemap[playerPos[1]][playerPos[0]] = DIRT
            # we go through all our number keys
            for key in controls:
                # if one was pressed by the player
                if event.key == controls[key]:
                    # if the mouse key was pressed in addition
                    if pygame.mouse.get_pressed()[0]:
                        # if this is a craftable resource
                        if key in craft:
                            # check that we have enough 
                            # ingredients to make the resource
                            canBeMade = True
                            for i in craft[key]:
                                if craft[key][i] > inventory[i]:
                                    canBeMade = False
                                    break
                            # if we can make the resource
                            # craft it and add it to our
                            # inventory
                            if canBeMade == True:
                                for i in craft[key]:
                                    inventory[i] -= craft[key][i]
                                inventory[key] += 1
                    # if no mouse key was pressed
                    # place this resource on the gorund
                    # and grab whatever is currently there
                    # into our inventory
                    else:
                        currentTile = tilemap[playerPos[1]][playerPos[0]]
                        if inventory[key] > 0:
                            inventory[key] -= 1
                            inventory[currentTile] += 1
                            tilemap[playerPos[1]][playerPos[0]] = key

    for row in range(MAPHEIGHT):
        for column in range(MAPWIDTH):
            DISPLAYSURF.blit(textures[tilemap[row][column]], (column*TILESIZE,row*TILESIZE))

    placePosition = 10
    for item in resources:
        DISPLAYSURF.blit(textures[item], (placePosition, MAPHEIGHT*TILESIZE+20))
        placePosition += 30
        textObj = INVFONT.render(str(inventory[item]), True, WHITE, BLACK)
        DISPLAYSURF.blit(textObj,(placePosition,MAPHEIGHT*TILESIZE+20))
        placePosition += 50

    DISPLAYSURF.blit(player,(playerPos[0]*TILESIZE, playerPos[1]*TILESIZE))
    pygame.display.update()