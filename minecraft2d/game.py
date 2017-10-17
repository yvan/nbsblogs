# http://usingpython.com/pygame-tilemaps/
# http://pygame.org/docs/tut/newbieguide.html
import sys
import random
import pygame

from pygame.locals import *

cloudx, cloudy = -200, 0
DIRT, GRASS, WATER, COAL, CLOUD, WOOD, FIRE, SAND, GLASS, ROCK, STONE, BRICK, DIAMOND = 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 , 10, 11, 12

WHITE = (255,255,255)
BLACK = (0,0,0)

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

controls = {
    DIRT: 49,
    GRASS: 50,
    WATER: 51,
    COAL: 52,
    WOOD: 53,
    FIRE: 54,
    SAND: 55,
    GLASS: 56,
    ROCK: 57,
    STONE: 48,
    BRICK: 45,
    DIAMOND: 61
}

craft = {
    FIRE: {WOOD: 2, ROCK: 2},
    STONE: {ROCK: 2},
    GLASS: {FIRE: 1, SAND:2},
    DIAMOND: {WOOD:2, COAL:3},
    BRICK: {ROCK:2, FIRE:1},
    SAND: {ROCK: 2}
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

TILESIZE = 40
MAPWIDTH = 30
MAPHEIGHT = 20

player = pygame.image.load('char.png')
playerPos = [0,0]

resources = [DIRT, GRASS, WATER, COAL, WOOD, FIRE, SAND, GLASS, ROCK, STONE, BRICK, DIAMOND]
tilemap = [[DIRT for w in range(MAPWIDTH)] for h in range(MAPHEIGHT)]

pygame.init()
DISPLAYSURF = pygame.display.set_mode((MAPWIDTH*TILESIZE,MAPHEIGHT*TILESIZE+50))
INVFONT = pygame.font.Font('freesansbold.ttf', 18)
pygame.display.set_caption('first game')
fpsClock = pygame.time.Clock()

for rw in range(MAPHEIGHT):
    for cl in range(MAPWIDTH):
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
        tilemap[rw][cl] = tile

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
            for key in controls:
                if event.key == controls[key]:
                    if pygame.mouse.get_pressed()[0]:
                        if key in craft:
                            canBeMade = True
                            for i in craft[key]:
                                if craft[key][i] > inventory[i]:
                                    canBeMade = False
                                    break
                            if canBeMade == True:
                                for i in craft[key]:
                                    inventory[i] -= craft[key][i]
                                inventory[key] += 1
                    else:
                        currentTile = tilemap[playerPos[1]][playerPos[0]]
                        if inventory[key] > 0:
                            inventory[key] -= 1
                            inventory[currentTile] += 1
                            tilemap[playerPos[1]][playerPos[0]] = key

    for row in range(MAPHEIGHT):
        for column in range(MAPWIDTH):
            DISPLAYSURF.blit(textures[tilemap[row][column]], (column*TILESIZE,row*TILESIZE))

    DISPLAYSURF.blit(player,(playerPos[0]*TILESIZE, playerPos[1]*TILESIZE))

    DISPLAYSURF.blit(textures[CLOUD].convert_alpha(), (cloudx, cloudy))
    cloudx += 1
    if cloudx > MAPWIDTH*TILESIZE:
        cloudy = random.randint(0, MAPHEIGHT*TILESIZE)
        cloudx = -200

    placePosition = 10
    for item in resources:
        DISPLAYSURF.blit(textures[item], (placePosition, MAPHEIGHT*TILESIZE+20))
        placePosition += 30
        textObj = INVFONT.render(str(inventory[item]), True, WHITE, BLACK)
        DISPLAYSURF.blit(textObj,(placePosition,MAPHEIGHT*TILESIZE+20))
        placePosition += 50
    pygame.display.update()
    fpsClock.tick()
