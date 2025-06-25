import pygame

class SideSelectionScreen:
    def __init__(self, screen, font):
        self.screen = screen
        self.font = font
        sw, sh = screen.get_size()

        self.button_width = 300
        self.button_height = 50
        self.spacing = 15
        self.section_gap = 50

        # Tính vị trí để nhóm 1 (chơi) và nhóm 2 (khác)
        total_height = (
            3 * self.button_height + 2 * self.spacing +  # nhóm chơi
            self.section_gap +
            2 * self.button_height + 1 * self.spacing    # nhóm khác
        )
        start_y = (sh - total_height) // 2

        # Group 1: game modes
        self.play_buttons = {
            "white":  pygame.Rect(sw//2 - self.button_width//2, start_y + 0 * (self.button_height + self.spacing), self.button_width, self.button_height),
            "black":  pygame.Rect(sw//2 - self.button_width//2, start_y + 1 * (self.button_height + self.spacing), self.button_width, self.button_height),
            "manual": pygame.Rect(sw//2 - self.button_width//2, start_y + 2 * (self.button_height + self.spacing), self.button_width, self.button_height)
        }

        # Group 2: utility
        util_start = start_y + 3 * (self.button_height + self.spacing) + self.section_gap
        self.utility_buttons = {
            "history": pygame.Rect(sw//2 - self.button_width//2, util_start + 0 * (self.button_height + self.spacing), self.button_width, self.button_height),
            "exit":    pygame.Rect(sw//2 - self.button_width//2, util_start + 1 * (self.button_height + self.spacing), self.button_width, self.button_height)
        }

    def draw(self):
        self.screen.fill((30, 30, 30))

        # Tiêu đề chính
        title_text = self.font.render("Chess Game", True, (255, 255, 255))
        self.screen.blit(title_text, (self.screen.get_width() // 2 - title_text.get_width() // 2, 50))

        # Tiêu đề nhóm
        sub1 = self.font.render("Choose a Game Mode", True, (180, 180, 180))
        self.screen.blit(sub1, (self.screen.get_width() // 2 - sub1.get_width() // 2, 120))

        sub2 = self.font.render("Other Options", True, (180, 180, 180))
        self.screen.blit(sub2, (self.screen.get_width() // 2 - sub2.get_width() // 2, self.utility_buttons["history"].top - 40))

        # Button rendering logic
        def draw_button(rect, label, text_color, bg_color):
            pygame.draw.rect(self.screen, bg_color, rect, border_radius=8)
            txt = self.font.render(label, True, text_color)
            self.screen.blit(txt, (rect.centerx - txt.get_width() // 2, rect.centery - txt.get_height() // 2))

        # Draw buttons in both groups
        button_labels = {
            "white":  ("Play as White",  (0, 0, 0),       (220, 220, 220)),
            "black":  ("Play as Black",  (255, 255, 255), (80, 80, 80)),
            "manual": ("Manual (2 Players)", (0, 0, 0),   (180, 180, 180)),
            "history":("View History",   (255, 255, 255), (70, 130, 180)),
            "exit":   ("Exit Game",      (255, 255, 255), (120, 50, 50)),
        }

        for key, rect in self.play_buttons.items():
            label, color, bg = button_labels[key]
            draw_button(rect, label, color, bg)

        for key, rect in self.utility_buttons.items():
            label, color, bg = button_labels[key]
            draw_button(rect, label, color, bg)

    def get_selection(self):
        while True:
            self.draw()
            pygame.display.flip()
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    return "exit"
                elif event.type == pygame.MOUSEBUTTONDOWN:
                    mx, my = pygame.mouse.get_pos()
                    for key, rect in {**self.play_buttons, **self.utility_buttons}.items():
                        if rect.collidepoint(mx, my):
                            return key
