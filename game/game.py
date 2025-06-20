import pygame
from game.board import Board
from game.player import Player
from game.ai_player import AIPlayer  # Import AIPlayer

WIDTH, HEIGHT = 640, 640

class Game:
    def __init__(self):
        pygame.init()
        self.screen = pygame.display.set_mode((WIDTH + 300, HEIGHT))
        pygame.display.set_caption("Chess with Prolog")
        self.font = pygame.font.SysFont("arial", 36)

        self.board = Board()

        self.players = {
            "white": Player("white", promotion_callback=self.ask_promotion_choice),
            #"black": Player("black", promotion_callback=self.ask_promotion_choice)
            "black": AIPlayer("black")
        }

        self.turn = "white"
        self.selected = None
        self.legal_moves = []
        self.running = True
        self.accept_move = True

    def ask_promotion_choice(self, turn, row, col):
        while True:
            self.screen.fill((50, 50, 50))
            self.board.draw(self.screen, [], None)
            self.board.draw_promotion_choices(self.screen, turn, row, col)
            pygame.display.flip()
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    pygame.quit()
                    exit()
                elif event.type == pygame.MOUSEBUTTONDOWN:
                    mx, my = pygame.mouse.get_pos()
                    for rect, piece in self.board.promotion_rects:
                        if rect.collidepoint(mx, my):
                            return piece

    def run(self):
        while self.running:
            self.board.draw(self.screen, self.legal_moves, self.selected)
            pygame.display.flip()
            if (self.accept_move):
                # AI TURN
                if isinstance(self.players[self.turn], AIPlayer):
                    self.turn, self.accept_move = self.players[self.turn].make_move(self.board)
                    continue

            # HUMAN TURN
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                        self.running = False

                elif event.type == pygame.MOUSEBUTTONDOWN and self.accept_move:
                    pos = pygame.mouse.get_pos()
                    row, col = self.board.get_square(pos)
                    player = self.players[self.turn]
                    self.selected, self.legal_moves, self.turn, self.accept_move = player.handle_click(
                        self.board, self.selected, row, col, self.turn
                    )

        pygame.quit()
