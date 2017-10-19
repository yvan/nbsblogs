import pygame

# declare the size of the map
# and the size of map tiles
# tile size is in pixels
# other sizes are in number of
# tiles
TILESIZE = 40
MAPWIDTH = 30
MAPHEIGHT = 20

# load the image for the player's 
# character
player = pygame.image.load('char.png')
# set the player's position 
# the first element controls left-right
# the second element controls up-down
playerPos = [0,0]

# initilaize the game object
pygame.init()
# intialize the display surface. this surface is what pygame draws
# things on
DISPLAYSURF = pygame.display.set_mode((MAPWIDTH*TILESIZE,MAPHEIGHT*TILESIZE))

# this loop runs once every few milliseconds
# executing the code inside it
while True:
    # place the player object (the loaded image)
    # on the tile in playerPos, [0, 0], multiplying by TILESIZE 
    # tells us to put the character at the right nummer of pixels
    DISPLAYSURF.blit(player,(playerPos[0]*TILESIZE, playerPos[1]*TILESIZE))
    # update the display every iteration of this loop
    pygame.display.update()