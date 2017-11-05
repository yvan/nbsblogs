# http://usingpython.com/pygame-tilemaps/
# http://pygame.org/docs/tut/newbieguide.html
import sys
import random
import pygame

from pygame.locals import *

GOLD, SILVER1, SILVER2, SILVER3, SILVER4, COBALT, COPPER, BRONZE = 0, 1, 2, 3, 4, 5, 6, 7
WATER, SHIP = 8, 9 

WHITE = (255,255,255)
BLACK = (0,0,0)

textures = {
    GOLD: pygame.transform.scale(pygame.image.load('resource_data/007.png'), (40,40)),
    SILVER1: pygame.transform.scale(pygame.image.load('resource_data/000.png'), (40,40)),
    SILVER2: pygame.transform.scale(pygame.image.load('resource_data/001.png'), (40,40)),
    SILVER3: pygame.transform.scale(pygame.image.load('resource_data/002.png'), (40,40)),
    SILVER4: pygame.transform.scale(pygame.image.load('resource_data/003.png'), (40,40)),
    COBALT: pygame.transform.scale(pygame.image.load('resource_data/004.png'), (40,40)),
    COPPER: pygame.transform.scale(pygame.image.load('resource_data/006.png'), (40,40)),
    BRONZE: pygame.transform.scale(pygame.image.load('resource_data/005.png'), (40,40)),
    WATER: pygame.transform.scale(pygame.image.load('water.png'), (40,40)),
    SHIP: pygame.transform.scale(pygame.image.load('smallships_data/000.png'), (40,40))
}

home_x, home_y = 10, 10
TILESIZE = 40
MAPWIDTH = 30
MAPHEIGHT = 20

tilemap = [[WATER for w in range(MAPWIDTH)] for h in range(MAPHEIGHT)]

pygame.init()
DISPLAYSURF = pygame.display.set_mode((MAPWIDTH*TILESIZE,MAPHEIGHT*TILESIZE))
INVFONT = pygame.font.Font('freesansbold.ttf', 18)
pygame.display.set_caption('galleon')

size = w,h = 40,40
colour = 50,205,50
home_rect = pygame.Rect((home_x, home_y), size)
home_image = pygame.Surface(size)
home_image.fill(colour)

for rw in range(MAPHEIGHT):
    for cl in range(MAPWIDTH):
        tile = WATER
        rn = random.randint(0,15)
        if rn in [0]:
            tile = GOLD
        elif rn in [1]:
            tile = SILVER1
        elif rn in [5]:
            tile = COBALT
        elif rn in [6]:
            tile = COPPER
        elif rn in [8]:
            tile = SILVER2
        elif rn in [9]:
            tile = SILVER3
        elif rn in [10]:
            tilef = SILVER4
        elif rn in [11]:
            tilef = BRONZE
        tilemap[rw][cl] = tile

while True:
    DISPLAYSURF.fill(BLACK)
    for event in pygame.event.get():
        if event.type == QUIT:
            pygame.quit()
            sys.exit()
        mouse = pygame.mouse.get_pressed()
        mouse_pos = pygame.mouse.get_pos()
        if mouse[0]:
            x_tile = int(round(mouse_pos[0]) / 40)
            y_tile = int(round(mouse_pos[1]) / 40) 
            current_tile = tilemap[y_tile][x_tile]
            if current_tile == WATER:
                tilemap[y_tile][x_tile] = SHIP

    for row in range(MAPHEIGHT):
        for column in range(MAPWIDTH):
            DISPLAYSURF.blit(textures[tilemap[row][column]], (column*TILESIZE,row*TILESIZE))

    DISPLAYSURF.blit(home_image, (home_x*TILESIZE, home_y*TILESIZE))
    pygame.display.update()
