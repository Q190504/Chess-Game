import os
import pygame
import copy
from game.board import Board
from game.alert import Alert
from utils.utils import parse_history_from_json
from logic.prolog_interface import move_piece

WIDTH, HEIGHT = 640, 640
ICON_SIZE = 30
PADDING = 50
MARGIN = 30

class HistoryViewer:
    def __init__(self, screen, font, game_history_str):
        self.screen = screen
        self.font = font
        self.board = Board()
        self.board.lim = 15
        data = parse_history_from_json(game_history_str)
        self.init_board = copy.deepcopy(data["init_grid"])
        self.move_history = copy.deepcopy(data["history"])

        # Precompute board states
        self.states = [(copy.deepcopy(self.init_board), None)]
        turn = "white"
        last_move = None
        rights = {
            "white_kingside": True,
            "white_queenside": True,
            "black_kingside": True,
            "black_queenside": True
        }
        grid = copy.deepcopy(self.init_board)
        for move in self.move_history:
            if len(move) > 1:
                fr, fc, tr, tc = move[:4]
                promo = move[5] if len(move) > 5 and isinstance(move[5], str) else None
                grid, turn, last_move, rights, _ = move_piece(
                    grid, (fr, fc), (tr, tc), turn, last_move, rights, promo
                )
            self.states.append((copy.deepcopy(grid), last_move))

        self.index = 0
        self.running = False

        # Load icons
        self.icons = {}
        for name in ("first", "prev", "next", "last"):
            path = os.path.join(os.path.dirname(__file__), "resource", f"{name}.png")
            surf = pygame.image.load(path).convert_alpha()
            self.icons[name] = pygame.transform.smoothscale(surf, (ICON_SIZE, ICON_SIZE))
        self.define_buttons()

    def draw_back_button(self):
        
        pygame.draw.rect(self.screen, (200, 100, 100), self.back_btn)
        back_text = self.font.render("Back", True, (255, 255, 255))
        self.screen.blit(back_text, (self.back_btn.centerx - back_text.get_width() // 2,
                                 self.back_btn.centery - back_text.get_height() // 2))
        
    def define_buttons(self):
        top = HEIGHT - PADDING
        x = WIDTH + PADDING
        self.first_btn = pygame.Rect(x, top, ICON_SIZE, ICON_SIZE)
        self.prev_btn  = pygame.Rect(x + ICON_SIZE + MARGIN, top, ICON_SIZE, ICON_SIZE)
        self.next_btn  = pygame.Rect(x + (ICON_SIZE + MARGIN) * 2, top, ICON_SIZE, ICON_SIZE)
        self.last_btn  = pygame.Rect(x + (ICON_SIZE + MARGIN) * 3, top, ICON_SIZE, ICON_SIZE)
        self.back_btn = pygame.Rect(WIDTH + 50, 20, 100, 30)

    def draw(self):
        self.screen.fill((30, 30, 30))
        board_grid, last_move = self.states[self.index]
        self.board.grid = board_grid
        self.board.last_move = last_move
        self.board.history = self.move_history[:self.index]
        self.board.draw(self.screen, [], [])
        self.board.draw_move_log(self.screen)
        self.draw_buttons()
        self.draw_back_button()
        pygame.display.flip()

    def draw_buttons(self):
        self.screen.blit(self.icons["first"], self.first_btn)
        self.screen.blit(self.icons["prev"],  self.prev_btn)
        self.screen.blit(self.icons["next"],  self.next_btn)
        self.screen.blit(self.icons["last"],  self.last_btn)

    def shutdown(self):
        pygame.quit()
        exit()

    def run(self):
        self.running = True
        clock = pygame.time.Clock()

        while self.running:
            self.draw()
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    self.shutdown()
                elif event.type == pygame.MOUSEBUTTONDOWN:
                    if event.button == 4:  # Scroll up
                        self.board.scroll_move_log(-1)
                    elif event.button == 5:  # Scroll down
                        self.board.scroll_move_log(1)
                    elif self.first_btn.collidepoint(event.pos):
                        self.index = 0
                        self.board.reset_move_log_scroll()
                    elif self.prev_btn.collidepoint(event.pos):
                        self.index = max(0, self.index - 1)
                        self.board.reset_move_log_scroll()
                    elif self.next_btn.collidepoint(event.pos):
                        self.index = min(len(self.states) - 1, self.index + 1)
                        self.board.reset_move_log_scroll()
                    elif self.last_btn.collidepoint(event.pos):
                        self.index = len(self.states) - 1
                        self.board.reset_move_log_scroll()
                    elif self.back_btn.collidepoint(event.pos):
                        def confirm():
                            self.running = False
                        def cancel():
                            pass
                        Alert(self.screen, self.font, "Back to history list?", 
                              [("Ok", confirm), ("Cancel", cancel)]).show()
            clock.tick(30)
