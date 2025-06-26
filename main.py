import os
import sys
import pygame

# Change directory to 'logic'
os.chdir(os.path.join(os.path.dirname(__file__), "logic"))
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), ".")))

from game.game import Game
from game.side_selector import SideSelectionScreen
from game.history_viewer import HistoryViewer
from game.history_selector import HistorySelectionScreen
WIDTH, HEIGHT = 640, 640

def main():
    pygame.init()
    screen = pygame.display.set_mode((WIDTH + 300, HEIGHT + 20))
    pygame.display.set_caption("Chess with Prolog")
    font = pygame.font.SysFont("arial", 20)
    root_dir = os.path.dirname(os.path.abspath(__file__))
    history_dir = os.path.abspath(os.path.join(root_dir, "saved_games"))

    while True:
        


        # After viewing history, choose side to play new game
        side_selector = SideSelectionScreen(screen, font)
        player_color = side_selector.get_selection()

        if player_color == "exit":
            break
        elif player_color == "history":
            # Select history file first
            selector = HistorySelectionScreen(screen, font, history_dir)
            selector.run()
        else:
            game = Game(screen=screen, font=font, player_color=player_color)
            game.run()
if __name__ == "__main__":
    main()
