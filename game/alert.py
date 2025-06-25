import pygame

class Alert:
    def __init__(self, screen, font, message, on_ok=None, on_cancel=None):
        self.screen = screen
        self.font = font
        self.message = message
        self.on_ok = on_ok
        self.on_cancel = on_cancel

        sw, sh = screen.get_size()
        self.rect = pygame.Rect(sw // 2 - 150, sh // 2 - 75, 300, 150)

        self.ok_button = pygame.Rect(self.rect.centerx - 90, self.rect.bottom - 50, 80, 35)
        self.cancel_button = pygame.Rect(self.rect.centerx + 10, self.rect.bottom - 50, 80, 35)

    def show(self):
        while True:
            pygame.draw.rect(self.screen, (60, 60, 60), self.rect)
            pygame.draw.rect(self.screen, (200, 200, 200), self.ok_button)
            pygame.draw.rect(self.screen, (200, 200, 200), self.cancel_button)

            # Render message
            text = self.font.render(self.message, True, (255, 255, 255))
            self.screen.blit(text, (self.rect.centerx - text.get_width() // 2, self.rect.top + 30))

            # Render OK text
            ok_text = self.font.render("OK", True, (0, 0, 0))
            self.screen.blit(ok_text, (
                self.ok_button.centerx - ok_text.get_width() // 2,
                self.ok_button.centery - ok_text.get_height() // 2
            ))

            # Render Cancel text
            cancel_text = self.font.render("Cancel", True, (0, 0, 0))
            self.screen.blit(cancel_text, (
                self.cancel_button.centerx - cancel_text.get_width() // 2,
                self.cancel_button.centery - cancel_text.get_height() // 2
            ))

            pygame.display.flip()

            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    pygame.quit()
                    exit()
                elif event.type == pygame.MOUSEBUTTONDOWN:
                    if self.ok_button.collidepoint(event.pos):
                        if self.on_ok:
                            self.on_ok()
                        return
                    elif self.cancel_button.collidepoint(event.pos):
                        if self.on_cancel:
                            self.on_cancel()
                        return
