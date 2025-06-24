import os
import sys
import pygame

# Change directory to 'logic'
os.chdir(os.path.join(os.path.dirname(__file__), "logic"))
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), ".")))

from game.game import Game
from game.side_selector import SideSelectionScreen

WIDTH, HEIGHT = 640, 640

def main():
    pygame.init()
    screen = pygame.display.set_mode((WIDTH + 300, HEIGHT))
    pygame.display.set_caption("Chess with Prolog")
    font = pygame.font.SysFont("arial", 20)

    # Show side selection
    selector = SideSelectionScreen(screen, font)
    player_color = selector.get_selection()

    # Start game with selected side
    game = Game(screen=screen, font=font, player_color=player_color)
    game.run()

if __name__ == "__main__":
    main()
