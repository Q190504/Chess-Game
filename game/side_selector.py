import pygame

class SideSelectionScreen:
    def __init__(self, screen, font):
        self.screen = screen
        self.font = font
        sw, sh = screen.get_size()
        self.white_button = pygame.Rect(sw//2 - 150, sh//2 - 60, 300, 50)
        self.black_button = pygame.Rect(sw//2 - 150, sh//2 + 10, 300, 50)
        self.manual_button = pygame.Rect(sw//2 - 150, sh//2 + 80, 300, 50)
        self.quit_button = pygame.Rect(sw//2 - 150, sh//2 + 150, 300, 40)

    def draw(self):
        self.screen.fill((30, 30, 30))
        title_text = self.font.render("Choose Your Side", True, (255, 255, 255))
        self.screen.blit(title_text, (self.screen.get_width() // 2 - title_text.get_width() // 2, 100))

        buttons = [
            (self.white_button, "Play as White", (0, 0, 0), (220, 220, 220)),
            (self.black_button, "Play as Black", (255, 255, 255), (80, 80, 80)),
            (self.manual_button, "Manual (2 players)", (0, 0, 0), (180, 180, 180)),
            (self.quit_button, "Exit Game", (255, 255, 255), (120, 50, 50))
        ]

        for rect, text, color, bg in buttons:
            pygame.draw.rect(self.screen, bg, rect)
            text_surf = self.font.render(text, True, color)
            self.screen.blit(text_surf, (rect.centerx - text_surf.get_width() // 2,
                                         rect.centery - text_surf.get_height() // 2))

    def get_selection(self):
        while True:
            self.draw()
            pygame.display.flip()
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    return "exit"
                elif event.type == pygame.MOUSEBUTTONDOWN:
                    mx, my = pygame.mouse.get_pos()
                    if self.white_button.collidepoint(mx, my):
                        return "white"
                    elif self.black_button.collidepoint(mx, my):
                        return "black"
                    elif self.manual_button.collidepoint(mx, my):
                        return "manual"
                    elif self.quit_button.collidepoint(mx, my):
                        return "exit"
