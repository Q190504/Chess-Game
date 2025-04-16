from logic.prolog_interface import get_legal_moves, move_piece

class Player:
    def __init__(self, color):
        self.color = color

    def is_own_piece(self, piece):
        return (self.color == "white" and piece.isupper()) or (self.color == "black" and piece.islower())

    def handle_click(self, board, selected, row, col, turn):
        piece = board.grid[row][col]
        legal_moves = []

        if selected:
            if (row, col) == selected:
                return None, [], turn  # Deselect
            elif piece != 'e' and self.is_own_piece(piece):
                return (row, col), get_legal_moves(board.grid, turn, row, col), turn
            else:
                board.grid, new_turn, moved = move_piece(board.grid, selected, (row, col), turn)
                if moved:
                    return None, [], new_turn
                return None, [], turn
        else:
            if piece != 'e' and self.is_own_piece(piece):
                return (row, col), get_legal_moves(board.grid, turn, row, col), turn
        return selected, legal_moves, turn
