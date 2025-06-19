import pygame
import os
from logic.prolog_interface import is_in_check

# Constants
WHITE = (240, 217, 181)
BROWN = (181, 136, 99)
GREEN = (0, 255, 0)
RED = (255, 0, 0)
SQ_SIZE = 80
PROMO_BOX_WIDTH = 120
PROMO_BOX_HEIGHT = 120
PROMO_GAP = 20
PROMO_START_X = 40
PROMO_START_Y = 200

class Board:
    def __init__(self):
        self.grid = [
            ['r','n','b','q','k','b','n','r'],
            ['p','p','p','p','p','p','p','p'],
            ['e','e','e','e','e','e','e','e'],
            ['e','e','e','e','e','e','e','e'],
            ['e','e','e','e','e','e','e','e'],
            ['e','e','e','e','e','e','e','e'],
            ['P','P','P','P','P','P','P','P'],
            ['R','N','B','Q','K','B','N','R']
        ]
        self.promotion_rects = []
        self.rights = {
            "white_kingside": True,
            "white_queenside": True,
            "black_kingside": True,
            "black_queenside": True
        }
        self.history = []
        self.last_move = []
        self.images = self.load_images()
        self.white_in_check = False
        self.black_in_check = False

    def load_images(self):
        piece_map = {
            'p': ('pawn', 'black'), 'P': ('pawn', 'white'),
            'r': ('rook', 'black'), 'R': ('rook', 'white'),
            'n': ('knight', 'black'), 'N': ('knight', 'white'),
            'b': ('bishop', 'black'), 'B': ('bishop', 'white'),
            'q': ('queen', 'black'), 'Q': ('queen', 'white'),
            'k': ('king', 'black'), 'K': ('king', 'white'),
        }

        images = {}
        for char, (name, color) in piece_map.items():
            path = os.path.join(os.path.dirname(__file__), "resource", f"{color}_{name}.png")
            images[char] = pygame.transform.scale(pygame.image.load(path), (SQ_SIZE, SQ_SIZE))
        return images

    def get_move_log_strings(self):
        log_lines = []
        for i in range(0, len(self.history), 2):
            white_move = self.history[i]
            white_str = self.format_move(white_move)

            black_str = ""
            if i + 1 < len(self.history):
                black_move = self.history[i + 1]
                black_str = self.format_move(black_move)

            move_num = i // 2 + 1
            log_line = f"{move_num}. {white_str}    {black_str}"
            log_lines.append(log_line)

        return log_lines

    def format_move(self, move):
        from_r, from_c, to_r, to_c = move[:4]
        piece = move[4]
        promo = move[5] if len(move) > 5 and isinstance(move[5], str) and len(move[5]) == 1 else None
        special = move[6] if len(move) > 6 else (move[5] if promo is None and len(move) > 5 else None)

        if special == 'castle_kingside':
            return "O-O"
        elif special == 'castle_queenside':
            return "O-O-O"
        else:
            from_sq = chr(from_c + ord('a')) + str(8 - from_r)
            to_sq = chr(to_c + ord('a')) + str(8 - to_r)
            move_str = f"{piece} {from_sq}→{to_sq}"
            if promo:
                move_str += f"={promo}"
            if special == 'en_passant':
                move_str += " ep"
            return move_str


    def draw_move_log(self, screen):
        font = pygame.font.SysFont("arial", 20)
        log_x = 8 * SQ_SIZE + 20
        log_y = 20
        line_spacing = 25
        
        pygame.draw.rect(screen, (0, 0, 0), (8 * SQ_SIZE, 0, 300, 8 * SQ_SIZE))

        log_lines = self.get_move_log_strings()
        for i, line in enumerate(log_lines[-20:]):  # only show last 20 moves
            text_surface = font.render(line, True, (255, 255, 255))
            screen.blit(text_surface, (log_x, log_y + i * line_spacing))

    def draw(self, screen, legal_moves, selected=None):
        highlight_surface = pygame.Surface((SQ_SIZE, SQ_SIZE), pygame.SRCALPHA)

        for r in range(8):
            for c in range(8):
                color = WHITE if (r + c) % 2 == 0 else BROWN
                pygame.draw.rect(screen, color, (c * SQ_SIZE, r * SQ_SIZE, SQ_SIZE, SQ_SIZE))
                piece = self.grid[r][c]
                if piece != 'e':
                    screen.blit(self.images[piece], (c * SQ_SIZE, r * SQ_SIZE))

        for r, c in legal_moves:
            target_piece = self.grid[r][c]
            if target_piece != 'e':
                highlight_surface.fill((RED[0], RED[1], RED[2], 100))
            else:
                highlight_surface.fill((GREEN[0], GREEN[1], GREEN[2], 100))
            screen.blit(highlight_surface, (c * SQ_SIZE, r * SQ_SIZE))

        if self.black_in_check:
            for r in range(8):
                for c in range(8):
                    if self.grid[r][c] == 'k':
                        pygame.draw.rect(screen, RED, (c * SQ_SIZE, r * SQ_SIZE, SQ_SIZE, SQ_SIZE), width=4)

        if self.white_in_check:
            for r in range(8):
                for c in range(8):
                    if self.grid[r][c] == 'K':
                        pygame.draw.rect(screen, RED, (c * SQ_SIZE, r * SQ_SIZE, SQ_SIZE, SQ_SIZE), width=4)

        if selected:
            r, c = selected
            pygame.draw.rect(screen, GREEN, (c * SQ_SIZE, r * SQ_SIZE, SQ_SIZE, SQ_SIZE), width=4)

        self.draw_move_log(screen)

    def draw_promotion_choices(self, screen, turn, row, col):
        self.promotion_rects.clear()
        choices = ['q', 'r', 'b', 'n']
        piece_case = str.upper if turn == 'white' else str.lower

        overlay = pygame.Surface((8 * SQ_SIZE, 8 * SQ_SIZE), pygame.SRCALPHA)
        overlay.fill((0, 0, 0, 180))
        screen.blit(overlay, (0, 0))
        pygame.draw.rect(screen, GREEN, (col * SQ_SIZE, row * SQ_SIZE, SQ_SIZE, SQ_SIZE), width=4)

        for i, ch in enumerate(choices):
            x = PROMO_START_X + i * (PROMO_BOX_WIDTH + PROMO_GAP)
            y = PROMO_START_Y
            rect = pygame.Rect(x, y, PROMO_BOX_WIDTH, PROMO_BOX_HEIGHT)
            pygame.draw.rect(screen, (220, 220, 220), rect)
            pygame.draw.rect(screen, (0, 0, 0), rect, 3)

            piece = piece_case(ch)
            if piece in self.images:
                piece_img = self.images[piece]
                img_x = rect.x + (PROMO_BOX_WIDTH - piece_img.get_width()) // 2
                img_y = rect.y + (PROMO_BOX_HEIGHT - piece_img.get_height()) // 2
                screen.blit(piece_img, (img_x, img_y))

            self.promotion_rects.append((rect, piece))

    def get_square(self, pos):
        x, y = pos
        return y // SQ_SIZE, x // SQ_SIZE

    def update_check_status(self):
        self.white_in_check = is_in_check(self.grid, 'white')
        self.black_in_check = is_in_check(self.grid, 'black')
