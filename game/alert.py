import pygame

class Alert:
    def __init__(self, screen, font, message, buttons, padding=20):
        """
        buttons: list of (label, callback) tuples
        """
        self.screen = screen
        self.font = font
        self.message = message
        self.buttons = buttons
        self.padding = padding

        # Measure message size
        self.text_surface = font.render(message, True, (255, 255, 255))
        text_width = self.text_surface.get_width()
        text_height = self.text_surface.get_height()

        # Button layout sizing
        self.button_width = 100
        self.button_height = 35
        button_spacing = 10
        total_button_width = len(buttons) * (self.button_width + button_spacing) - button_spacing

        # Alert box width should fit both message and button row
        content_width = max(text_width, total_button_width)
        self.box_width = max(300, content_width + 2 * padding)
        self.box_height = text_height + 90

        sw, sh = screen.get_size()
        self.rect = pygame.Rect(
            (sw - self.box_width) // 2,
            (sh - self.box_height) // 2,
            self.box_width,
            self.box_height
        )

        # Compute button positions
        self.button_rects = []
        start_x = self.rect.centerx - total_button_width // 2
        y = self.rect.bottom - 50

        for i, (label, _) in enumerate(buttons):
            rect = pygame.Rect(start_x + i * (self.button_width + button_spacing), y, self.button_width, self.button_height)
            self.button_rects.append((rect, label))

    def show(self):
        while True:
            pygame.draw.rect(self.screen, (60, 60, 60), self.rect, border_radius=8)

            # Draw message
            self.screen.blit(
                self.text_surface,
                (self.rect.centerx - self.text_surface.get_width() // 2, self.rect.top + 20)
            )

            # Draw buttons
            for rect, label in self.button_rects:
                pygame.draw.rect(self.screen, (200, 200, 200), rect, border_radius=5)
                label_surf = self.font.render(label, True, (0, 0, 0))
                self.screen.blit(label_surf, (
                    rect.centerx - label_surf.get_width() // 2,
                    rect.centery - label_surf.get_height() // 2
                ))

            pygame.display.flip()

            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    pygame.quit()
                    exit()
                elif event.type == pygame.MOUSEBUTTONDOWN:
                    for i, (rect, _) in enumerate(self.button_rects):
                        if rect.collidepoint(event.pos):
                            _, callback = self.buttons[i]
                            if callback:
                                callback()
                            return
