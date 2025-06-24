import pygame

class SideSelectionScreen:
    def __init__(self, screen, font):
        self.screen = screen
        self.font = font

        self.screen_width, self.screen_height = self.screen.get_size()
        button_width = self.screen_width * 0.4
        button_height = self.screen_height * 0.1

        center_x = self.screen_width // 2
        start_y = self.screen_height * 0.35

        self.white_button = pygame.Rect(0, 0, button_width, button_height)
        self.white_button.center = (center_x, start_y)

        self.black_button = pygame.Rect(0, 0, button_width, button_height)
        self.black_button.center = (center_x, start_y + button_height + 20)

        self.manual_button = pygame.Rect(0, 0, button_width, button_height)
        self.manual_button.center = (center_x, start_y + 2 * (button_height + 20))

    def draw(self):
        self.screen.fill((30, 30, 30))

        title_text = self.font.render("Choose Game Mode", True, (255, 255, 255))
        self.screen.blit(title_text, (
            self.screen_width // 2 - title_text.get_width() // 2,
            self.screen_height * 0.2))

        pygame.draw.rect(self.screen, (220, 220, 220), self.white_button)
        pygame.draw.rect(self.screen, (80, 80, 80), self.black_button)
        pygame.draw.rect(self.screen, (100, 100, 200), self.manual_button)

        white_text = self.font.render("Play as White", True, (0, 0, 0))
        black_text = self.font.render("Play as Black", True, (255, 255, 255))
        manual_text = self.font.render("Manual Mode", True, (255, 255, 255))

        self.screen.blit(white_text, (
            self.white_button.centerx - white_text.get_width() // 2,
            self.white_button.centery - white_text.get_height() // 2))

        self.screen.blit(black_text, (
            self.black_button.centerx - black_text.get_width() // 2,
            self.black_button.centery - black_text.get_height() // 2))

        self.screen.blit(manual_text, (
            self.manual_button.centerx - manual_text.get_width() // 2,
            self.manual_button.centery - manual_text.get_height() // 2))

    def get_selection(self):
        while True:
            self.draw()
            pygame.display.flip()
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    pygame.quit()
                    exit()
                elif event.type == pygame.MOUSEBUTTONDOWN:
                    mx, my = pygame.mouse.get_pos()
                    if self.white_button.collidepoint(mx, my):
                        return "white"
                    elif self.black_button.collidepoint(mx, my):
                        return "black"
                    elif self.manual_button.collidepoint(mx, my):
                        return "manual"
