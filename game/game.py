import pygame
import threading
import time
import os
from game.board import Board
from game.player import Player
from game.ai_player import AIPlayer
from pyswip import Prolog

WIDTH, HEIGHT = 640, 640

class Game:
    def __init__(self, screen, font, player_color):
        self.screen = screen
        self.font = font
        self.board = Board()
        
        # Choose player vs AI based on selection
        if player_color == "manual":
            self.players = {
                "white": Player("white", promotion_callback=self.ask_promotion_choice),
                "black": Player("black", promotion_callback=self.ask_promotion_choice)
            }
        else:
            ai_color = "black" if player_color == "white" else "white"
            self.players = {
                player_color: Player(player_color, promotion_callback=self.ask_promotion_choice),
                ai_color: AIPlayer(ai_color)
            }

        self.turn = "white"
        self.selected = None
        self.legal_moves = []
        self.running = True
        self.accept_move = True
        self.ai_thread = None
        self.ai_start_time = None

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

    def run_ai_move(self):
        """Threaded function to let AI make its move."""
        if self.running:
            self.turn, self.accept_move = self.players[self.turn].make_move(self.board)
            self.ai_thread = None
            self.ai_start_time = None  # Clear timer when done

    def draw_thinking_overlay(self):
        """Draw 'Thinking...' message and elapsed time."""
        if self.ai_start_time:
            elapsed_sec = int(time.time() - self.ai_start_time)
            timer_text = self.font.render(f"AI {self.turn} thinking...{elapsed_sec} sec", True, (255, 255, 255))
            self.screen.blit(timer_text, (WIDTH + 10, HEIGHT - 60))
        else:
            timer_text = self.font.render(f"This turn: {self.turn}", True, (255, 255, 255))
            self.screen.blit(timer_text, (WIDTH + 10, HEIGHT - 60))

    def shutdown(self):
        if self.ai_thread and self.ai_thread.is_alive():
            # Optionally wait briefly to avoid resource corruption
            self.ai_thread.join(timeout=0.5)
        pygame.quit()
        try:
            Prolog().query("halt.")
        except:
            pass
        os._exit(0)

    def run(self):
        while self.running:
            self.board.draw(self.screen, self.legal_moves, self.selected)

            self.draw_thinking_overlay()

            pygame.display.flip()

            # Start AI thread if needed
            if self.accept_move and self.ai_thread is None:
                if isinstance(self.players[self.turn], AIPlayer):
                    self.accept_move = False
                    self.ai_start_time = time.time()
                    self.ai_thread = threading.Thread(target=self.run_ai_move)
                    self.ai_thread.start()

            # Handle user events
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    self.running = False

                elif event.type == pygame.MOUSEBUTTONDOWN and self.accept_move and isinstance(self.players[self.turn], Player):
                    pos = pygame.mouse.get_pos()
                    row, col = self.board.get_square(pos)
                    player = self.players[self.turn]
                    self.selected, self.legal_moves, self.turn, self.accept_move = player.handle_click(
                        self.board, self.selected, row, col, self.turn
                    )

        self.shutdown()
