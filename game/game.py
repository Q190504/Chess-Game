import pygame
from game.board import Board
from game.player import Player

WIDTH, HEIGHT = 640, 640

class Game:
    def __init__(self):
        pygame.init()
        self.screen = pygame.display.set_mode((WIDTH, HEIGHT))
        pygame.display.set_caption("Chess with Prolog")
        self.font = pygame.font.SysFont("arial", 36)

        self.board = Board()
        self.players = {
            "white": Player("white"),
            "black": Player("black")
        }

        self.turn = "white"
        self.selected = None
        self.legal_moves = []
        self.running = True

    def run(self):
        while self.running:
            self.board.draw(self.screen, self.legal_moves, self.font, self.selected)
            pygame.display.flip()

            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    self.running = False

                elif event.type == pygame.MOUSEBUTTONDOWN:
                    pos = pygame.mouse.get_pos()
                    row, col = self.board.get_square(pos)
                    player = self.players[self.turn]
                    self.selected, self.legal_moves, self.turn = player.handle_click(
                        self.board, self.selected, row, col, self.turn
                    )

        pygame.quit()
