import pygame
import os

# Color const
WHITE = (240, 217, 181)
BROWN = (181, 136, 99)
GREEN = (0, 255, 0)
RED = (255, 0, 0)
SQ_SIZE = 80

class Board:
    def __init__(self):
        self.grid = [
            ['r', 'n', 'b', 'q', 'k', 'b', 'n', 'r'],
            ['p'] * 8,
            ['e'] * 8,
            ['e'] * 8,
            ['e'] * 8,
            ['e'] * 8,
            ['P'] * 8,
            ['R', 'N', 'B', 'Q', 'K', 'B', 'N', 'R']
        ]
        self.last_move = []
        self.images = self.load_images()

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

    def draw(self, screen, legal_moves, font, selected=None):
        highlight_surface = pygame.Surface((SQ_SIZE, SQ_SIZE), pygame.SRCALPHA)

        # Draw the board
        for r in range(8):
            for c in range(8):
                color = WHITE if (r + c) % 2 == 0 else BROWN
                pygame.draw.rect(screen, color, (c * SQ_SIZE, r * SQ_SIZE, SQ_SIZE, SQ_SIZE))

                piece = self.grid[r][c]
                if piece != 'e':
                    screen.blit(self.images[piece], (c * SQ_SIZE, r * SQ_SIZE))

        # Highlight legal moves, red as targeted piece
        for r, c in legal_moves:
            target_piece = self.grid[r][c]
            if target_piece != 'e':
                highlight_surface.fill((RED[0], RED[1], RED[2], 100))
            else:
                highlight_surface.fill((GREEN[0], GREEN[1], GREEN[2], 100))
            screen.blit(highlight_surface, (c * SQ_SIZE, r * SQ_SIZE))

        # Highlight selected square
        if selected:
            r, c = selected
            pygame.draw.rect(screen, GREEN, (c * SQ_SIZE, r * SQ_SIZE, SQ_SIZE, SQ_SIZE), width=4)

    def get_square(self, pos):
        x, y = pos
        return y // SQ_SIZE, x // SQ_SIZE
