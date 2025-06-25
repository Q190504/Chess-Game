import os
import pygame
from game.history_viewer import HistoryViewer
from game.alert import Alert
class HistorySelectionScreen:
    def __init__(self, screen, font, history_dir):
        self.screen = screen
        self.font = font
        self.history_dir = history_dir
        self.files = sorted([f for f in os.listdir(history_dir) if f.endswith(".json")])

        self.item_height = 40
        self.top_margin = 100
        self.bg_color = (30, 30, 30)
        self.text_color = (220, 220, 220)
        self.hover_color = (100, 160, 210)
        self.back_button = pygame.Rect(50, 20, 100, 30)

        sw, sh = screen.get_size()
        self.width = sw
        self.height = sh

        self.visible_items = (self.height - self.top_margin - 60) // self.item_height
        self.scroll_offset = 0

    def draw(self):
        self.screen.fill(self.bg_color)
        title = self.font.render("Select a History to View", True, (255, 255, 255))
        self.screen.blit(title, (self.width // 2 - title.get_width() // 2, 30))

        mx, my = pygame.mouse.get_pos()

        start = self.scroll_offset
        end = min(start + self.visible_items, len(self.files))

        for i in range(start, end):
            filename = self.files[i]
            y = self.top_margin + (i - start) * self.item_height
            rect = pygame.Rect(50, y, self.width - 100, self.item_height - 5)

            if rect.collidepoint(mx, my):
                pygame.draw.rect(self.screen, self.hover_color, rect)
                text_color = (255, 255, 255)
            else:
                text_color = self.text_color

            text_surf = self.font.render(filename, True, text_color)
            self.screen.blit(text_surf, (60, y + 5))

        instr = self.font.render("Scroll to navigate. Click to open. ESC to quit.", True, (200, 200, 200))
        self.screen.blit(instr, (50, self.height - 50))

    def draw_back_button(self):
        pygame.draw.rect(self.screen, (200, 100, 100), self.back_button)
        back_text = self.font.render("Back", True, (255, 255, 255))
        self.screen.blit(back_text, (self.back_button.centerx - back_text.get_width() // 2,
                                 self.back_button.centery - back_text.get_height() // 2))
    def shutdown(self):
        pygame.quit()
        exit()

    def run(self):
        clock = pygame.time.Clock()
        while True:
            self.draw()
            self.draw_back_button()
            pygame.display.flip()

            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    self.shutdown()

                elif event.type == pygame.KEYDOWN:
                    if event.key == pygame.K_ESCAPE:
                        return

                elif event.type == pygame.MOUSEBUTTONDOWN:
                    if event.button == 1:  # Left click
                        if self.back_button.collidepoint(event.pos):
                            return
                        mx, my = event.pos
                        clicked_index = self.get_clicked_index(mx, my)
                        if clicked_index is not None:
                            if not self.open_history(clicked_index):
                                return
                    elif event.button == 4:  # Scroll up
                        self.scroll_offset = max(0, self.scroll_offset - 1)
                    elif event.button == 5:  # Scroll down
                        max_scroll = max(0, len(self.files) - self.visible_items)
                        self.scroll_offset = min(max_scroll, self.scroll_offset + 1)
                    
            clock.tick(30)

    def get_clicked_index(self, mx, my):
        start = self.scroll_offset
        end = min(start + self.visible_items, len(self.files))
        for i in range(start, end):
            y = self.top_margin + (i - start) * self.item_height
            rect = pygame.Rect(50, y, self.width - 100, self.item_height - 5)
            if rect.collidepoint(mx, my):
                return i
        return None

    def open_history(self, index):
        filename = self.files[index]
        path = os.path.join(self.history_dir, filename)
        try:
            with open(path, "r") as f:
                json_str = f.read()
            hv = HistoryViewer(self.screen, self.font, json_str)
            hv.run()  # block until closed
            return True
        except Exception as e:
            print(f"Failed to open {filename}: {e}")
            Alert(self.screen, self.font, f"Failed to open {filename}: {e}", 
                  [("Ok", None)]).show()
            return True  # keep running even if error
