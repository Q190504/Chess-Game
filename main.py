import os
import sys

# Change directory to 'logic'
os.chdir(os.path.join(os.path.dirname(__file__), "logic"))
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), ".")))

from game.game import Game

if __name__ == "__main__":
    game = Game()
    game.run()
