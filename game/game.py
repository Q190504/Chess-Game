import pygame
import time
from game.board import Board
from game.player import Player
from game.ai_player import AIPlayer
from game.alert import Alert
from utils.utils import save_history_to_json

WIDTH, HEIGHT = 640, 640

class Game:
    def __init__(self, screen, font, player_color):
        self.screen = screen
        self.font = font
        self.board = Board()
        
        # Choose player vs AI based on selection
        if player_color == "manual":
            self.players = {
                "white": Player("white", board=self.board, promotion_callback=self.ask_promotion_choice),
                "black": Player("black", board=self.board, promotion_callback=self.ask_promotion_choice)
            }
        else:
            ai_color = "black" if player_color == "white" else "white"
            self.players = {
                player_color: Player(player_color, board=self.board, promotion_callback=self.ask_promotion_choice),
                ai_color: AIPlayer(ai_color, board=self.board)
            }

        self.turn = "white"
        self.selected = None
        self.legal_moves = []
        self.running = True
        self.accept_move = True
        self.ai_start_time = None
        self.back_button = pygame.Rect(WIDTH + 50, 20, 100, 30)
    
    def draw_back_button(self):
        
        pygame.draw.rect(self.screen, (200, 100, 100), self.back_button)
        back_text = self.font.render("Back", True, (255, 255, 255))
        self.screen.blit(back_text, (self.back_button.centerx - back_text.get_width() // 2,
                                 self.back_button.centery - back_text.get_height() // 2))
        
    def ask_promotion_choice(self, turn, row, col):
        while True:
            self.screen.fill((50, 50, 50))
            self.board.draw(self.screen, [], None)
            self.board.draw_promotion_choices(self.screen, turn, row, col)
            pygame.display.flip()
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    self.shutdown()
                elif event.type == pygame.MOUSEBUTTONDOWN:
                    mx, my = pygame.mouse.get_pos()
                    for rect, piece in self.board.promotion_rects:
                        if rect.collidepoint(mx, my):
                            return piece

    def draw_thinking_overlay(self):
        """Draw 'Thinking...' message and elapsed time."""
        if self.ai_start_time:
            elapsed_sec = int(time.time() - self.ai_start_time)
            timer_text = self.font.render(f"AI {self.turn} thinking...{elapsed_sec} sec", True, (255, 255, 255))
            self.screen.blit(timer_text, (WIDTH + 50, 50 + timer_text.get_height()//2))
        else:
            timer_text = self.font.render(f"This turn: {self.turn}", True, (255, 255, 255))
            self.screen.blit(timer_text, (WIDTH + 50, 50 + timer_text.get_height()//2))

    def clean_up(self):
        if isinstance(self.players["white"], AIPlayer):
            self.players["white"].terminate_task()
        if isinstance(self.players["black"], AIPlayer):
            self.players["black"].terminate_task()

    def shutdown(self):
        self.clean_up()
        pygame.quit()
        exit()

    def run(self):
        while self.running:
            self.board.draw(self.screen, self.legal_moves, self.selected)

            self.draw_thinking_overlay()
            self.draw_back_button()
            pygame.display.flip()

            prev_turn = self.turn
            # Start AI thread if needed
            if self.accept_move:
                if isinstance(self.players[self.turn], AIPlayer):
                    self.ai_start_time = self.players[self.turn].ai_start_time
                    self.turn, self.accept_move = self.players[self.turn].make_move()
                else:
                    self.ai_start_time = None
            else:
                self.ai_start_time = None
            # Handle user events
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    self.shutdown()

                elif event.type == pygame.MOUSEBUTTONDOWN:
                    if event.button == 4:  # Scroll up
                        self.board.scroll_move_log(-1)
                    elif event.button == 5:  # Scroll down
                        self.board.scroll_move_log(1)
                    elif self.back_button.collidepoint(event.pos):
                        def on_confirm():
                            if not self.accept_move:
                                save_history_to_json(self)
                            self.running = False
                            self.accept_move = False
                            self.clean_up()

                        def on_cancel():
                            # Just resume game
                            pass

                        alert = Alert(self.screen, self.font, "Exit to menu?", on_ok=on_confirm, on_cancel=on_cancel)
                        alert.show()

                    elif self.accept_move and isinstance(self.players[self.turn], Player):
                        pos = pygame.mouse.get_pos()
                        row, col = self.board.get_square(pos)
                        player = self.players[self.turn]
                        self.selected, self.legal_moves, self.turn, self.accept_move = player.handle_click(
                            self.selected, row, col, self.turn
                        )

            if (prev_turn != self.turn):
                self.board.reset_move_log_scroll()
                    
                    

        return
