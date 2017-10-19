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

# create starting positions for clouds
cloudxs, cloudys = [[-500, MAPWIDTH*TILESIZE+500], [0, 250]]
bird_pos = [-100, 450]

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

# create an fps clock (this just slows down our game a little)
# to make the clouds move naturally
fpsClock = pygame.time.Clock()

# define a function that tells
# us which bird image to render next
# therer are 3 that make the bird look
# like it flies
def which_bird(bird_count):
    if bird_count == 1:
        return 2, pygame.image.load('bird2.png')
    elif bird_count == 2:
        return 3, pygame.image.load('bird3.png')
    else:
        return 1, pygame.image.load('bird1.png')
# set the first bird image to bird1.png
current_bird = 1

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

    # make sure to render player before the
    # clouds or you will walk on top of the clouds
    DISPLAYSURF.blit(player,(playerPos[0]*TILESIZE, playerPos[1]*TILESIZE))


    # render the birds
    current_bird, bird_image = which_bird(current_bird)
    DISPLAYSURF.blit(bird_image, (bird_pos[0], bird_pos[1]))

    # update the bird position
    bird_pos[0] += 3

    if bird_pos[0] > MAPWIDTH*TILESIZE:
        bird_pos[1] = random.randint(0, MAPHEIGHT*TILESIZE)
        bird_pos[0] = -100

    # display both clouds
    DISPLAYSURF.blit(textures[CLOUD], (cloudxs[0], cloudys[0]))
    DISPLAYSURF.blit(textures[CLOUD], (cloudxs[1], cloudys[1]))
    # incrementally move the clouds
    cloudxs[0] += 1
    cloudxs[1] -= 1
    # if cloud 0 moves beyond map width, reset the cloud 
    # to a random place
    if cloudxs[0] > MAPWIDTH*TILESIZE:
        cloudys[0] = random.randint(0, MAPHEIGHT*TILESIZE)
        cloudxs[0] = -500
    # if cloud 1 moves beyond map width, reset the cloud 
    # to a random place
    if cloudxs[1] < 0 :
        cloudys[1] = random.randint(0, MAPHEIGHT*TILESIZE)
        cloudxs[1] = MAPWIDTH*TILESIZE+500

    placePosition = 10
    for item in resources:
        DISPLAYSURF.blit(textures[item], (placePosition, MAPHEIGHT*TILESIZE+20))
        placePosition += 30
        textObj = INVFONT.render(str(inventory[item]), True, WHITE, BLACK)
        DISPLAYSURF.blit(textObj,(placePosition,MAPHEIGHT*TILESIZE+20))
        placePosition += 50

    pygame.display.update()
    # call the fps clock
    # again this jus slows down the
    # game a little to make the clouds
    # move slower
    fpsClock.tick()