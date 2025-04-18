from logic.prolog_interface import get_legal_moves, move_piece, is_checkmate, is_stalemate

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
                return (row, col), get_legal_moves(board.grid, turn, row, col, board.last_move), turn
            else:
                board.grid, new_turn, board.last_move, moved = move_piece(board.grid, selected, (row, col), turn, board.last_move)
                if moved:
                    if is_checkmate(board.grid, new_turn, board.last_move):
                        print(f"Checkmate! {turn.capitalize()} wins.")
                    elif is_stalemate(board.grid, new_turn, board.last_move):
                        print("Stalemate! It's a draw.")
                    return None, [], new_turn
                return None, [], turn
        else:
            if piece != 'e' and self.is_own_piece(piece):
                return (row, col), get_legal_moves(board.grid, turn, row, col, board.last_move), turn
        return selected, legal_moves, turn
