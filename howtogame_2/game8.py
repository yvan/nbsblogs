import sys
import json
import random
import pygame
import argparse

from datetime import datetime
from pygame.locals import *

DIRT, GRASS, WATER, COAL, CLOUD, WOOD = 0, 1, 2, 3, 4, 5
FIRE, SAND, GLASS, ROCK, STONE, BRICK, DIAMOND = 6, 7, 8, 9 , 10, 11, 12
resources = [DIRT, GRASS, WATER, COAL, WOOD, FIRE, SAND, GLASS, ROCK, STONE, BRICK, DIAMOND]
WHITE = (255,255,255)
BLACK = (0,0,0)
TILESIZE = 40
MAPWIDTH = 30
MAPHEIGHT = 20

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

def setup_game(load_file=None):
    if load_file is None:
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
        game_state = {}
        game_state['current_bird'] = 1
        game_state['bird_pos'] = bird_pos
        game_state['playerPos'] = playerPos
        game_state['tilemap'] = tilemap
        game_state['cloudxs'] = cloudxs
        game_state['cloudys'] = cloudys
        game_state['inventory'] = inventory
        return game_state

    else:
        with open(load_file, 'r') as f:
            game_state = json.load(f)
            game_state['inventory'] = {int(k):v for k,v in game_state['inventory'].items()}
            return game_state

def which_bird(bird_count):
    if bird_count == 1:
        return 2, pygame.image.load('bird2.png')
    elif bird_count == 2:
        return 3, pygame.image.load('bird3.png')
    else:
        return 1, pygame.image.load('bird1.png')

# saves just our tiles into
# a file
def save_objective(game_state):
    save_date = datetime.now().strftime('%Y-%m-%d-%H-%M-%S')
    with open(f'objective-{save_date}.save', 'w') as f:
        json.dump(game_state['tilemap'], f)

def parse_args(args):
    parser = argparse.ArgumentParser()
    parser.add_argument('-s', '--savegame', dest='savegame', help='The savegame file name to load.')
    parser.add_argument('-o', '--objective', dest='objective', help='The objective for this game.')
    return parser.parse_args(args)

if __name__ == '__main__':
    args = parse_args(sys.argv[1:]).__dict__
    if 'savegame' in args.keys():
        game_state = setup_game(load_file=args['savegame'])
    else:
        game_state = setup_game()

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

    pygame.init()
    DISPLAYSURF = pygame.display.set_mode((MAPWIDTH*TILESIZE,MAPHEIGHT*TILESIZE+50))
    INVFONT = pygame.font.Font('freesansbold.ttf', 18)
    fpsClock = pygame.time.Clock()

    while True:
        DISPLAYSURF.fill(BLACK)
        # check for keys being held, returns
        # all keys set to 0 with 1 for pressed
        # keys
        display_objective = False
        allkeys = pygame.key.get_pressed()
        if allkeys[K_f]:
            display_objective = True
        for event in pygame.event.get():
            if event.type == QUIT:
                pygame.quit()
                sys.exit()
            elif event.type == KEYDOWN:
                if event.key == K_s:
                    save_date = datetime.now().strftime('%Y-%m-%d-%H-%M-%S')
                    with open(f'save-file-{save_date}.save', 'w') as f:
                        json.dump(game_state, f)
                # press o to save the tilemap as an objective
                if event.key == K_o:
                    save_objective(game_state)
                if event.key == K_RIGHT and game_state['playerPos'][0] < MAPWIDTH - 1:
                    game_state['playerPos'][0] += 1
                if event.key == K_LEFT and game_state['playerPos'][0] > 0:
                    game_state['playerPos'][0] -= 1
                if event.key == K_DOWN and game_state['playerPos'][1] < MAPHEIGHT - 1:
                    game_state['playerPos'][1] += 1
                if event.key == K_UP and game_state['playerPos'][1] > 0:
                    game_state['playerPos'][1] -= 1
                if event.key == K_SPACE:
                    currentTile = game_state['tilemap'][game_state['playerPos'][1]][game_state['playerPos'][0]]
                    game_state['inventory'][currentTile] += 1
                    game_state['tilemap'][game_state['playerPos'][1]][game_state['playerPos'][0]] = DIRT
                for key in controls:
                    if event.key == controls[key]:
                        if pygame.mouse.get_pressed()[0]:
                            if key in craft:
                                canBeMade = True
                                for i in craft[key]:
                                    if craft[key][i] > game_state['inventory'][i]:
                                        canBeMade = False
                                        break
                                if canBeMade == True:
                                    for i in craft[key]:
                                        game_state['inventory'][i] -= craft[key][i]
                                    game_state['inventory'][key] += 1
                        else:
                            currentTile = game_state['tilemap'][game_state['playerPos'][1]][game_state['playerPos'][0]]
                            if game_state['inventory'][key] > 0:
                                game_state['inventory'][key] -= 1
                                game_state['inventory'][currentTile] += 1
                                game_state['tilemap'][game_state['playerPos'][1]][game_state['playerPos'][0]] = key

        # if we are holding f render the guidelines
        if display_objective and args['objective'] is not None:
            # load guidelines from disk
            with open(args['objective'], 'r') as f:
                objective_overlay = json.load(f)
                # render them
                for row in range(MAPHEIGHT):
                    for column in range(MAPWIDTH):
                        DISPLAYSURF.blit(textures[objective_overlay[row][column]], (column*TILESIZE,row*TILESIZE))

        # otherwise display our current map
        else:
            for row in range(MAPHEIGHT):
                for column in range(MAPWIDTH):
                    DISPLAYSURF.blit(textures[game_state['tilemap'][row][column]], (column*TILESIZE,row*TILESIZE))

        DISPLAYSURF.blit(player,(game_state['playerPos'][0]*TILESIZE, game_state['playerPos'][1]*TILESIZE))

        game_state['current_bird'], bird_image = which_bird(game_state['current_bird'])
        DISPLAYSURF.blit(bird_image, (game_state['bird_pos'][0], game_state['bird_pos'][1]))

        game_state['bird_pos'][0] += 3

        if game_state['bird_pos'][0] > MAPWIDTH*TILESIZE:
            game_state['bird_pos'][1] = random.randint(0, MAPHEIGHT*TILESIZE)
            game_state['bird_pos'][0] = -100

        DISPLAYSURF.blit(textures[CLOUD], (game_state['cloudxs'][0], game_state['cloudys'][0]))
        DISPLAYSURF.blit(textures[CLOUD], (game_state['cloudxs'][1], game_state['cloudys'][1]))

        game_state['cloudxs'][0] += 1
        game_state['cloudxs'][1] -= 1

        if game_state['cloudxs'][0] > MAPWIDTH*TILESIZE:
            game_state['cloudys'][0] = random.randint(0, MAPHEIGHT*TILESIZE)
            game_state['cloudxs'][0] = -500

        if game_state['cloudxs'][1] < 0 :
            game_state['cloudys'][1] = random.randint(0, MAPHEIGHT*TILESIZE)
            game_state['cloudxs'][1] = MAPWIDTH*TILESIZE+500

        placePosition = 10
        for item in resources:
            DISPLAYSURF.blit(textures[item], (placePosition, MAPHEIGHT*TILESIZE+20))
            placePosition += 30
            textObj = INVFONT.render(str(game_state['inventory'][item]), True, WHITE, BLACK)
            DISPLAYSURF.blit(textObj,(placePosition,MAPHEIGHT*TILESIZE+20))
            placePosition += 50

        pygame.display.update()
        fpsClock.tick()
