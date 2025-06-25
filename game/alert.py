import pygame

class Alert:
    def __init__(self, screen, font, message):
        self.screen = screen
        self.font = font
        self.message = message
        sw, sh = screen.get_size()
        self.rect = pygame.Rect(sw // 2 - 150, sh // 2 - 50, 300, 100)
        self.ok_button = pygame.Rect(self.rect.centerx - 40, self.rect.bottom - 40, 80, 30)

    def show(self):
        while True:
            pygame.draw.rect(self.screen, (60, 60, 60), self.rect)
            pygame.draw.rect(self.screen, (180, 180, 180), self.ok_button)

            text = self.font.render(self.message, True, (255, 255, 255))
            self.screen.blit(text, (self.rect.centerx - text.get_width() // 2, self.rect.top + 20))

            ok_text = self.font.render("OK", True, (0, 0, 0))
            self.screen.blit(ok_text, (self.ok_button.centerx - ok_text.get_width() // 2,
                                       self.ok_button.centery - ok_text.get_height() // 2))

            pygame.display.flip()

            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    pygame.quit()
                    exit()
                elif event.type == pygame.MOUSEBUTTONDOWN:
                    if self.ok_button.collidepoint(event.pos):
                        return
