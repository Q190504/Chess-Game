from logic.prolog_interface import get_legal_moves, move_piece, is_checkmate, is_stalemate
class Player:
    def __init__(self, color, promotion_callback=None):
        self.color = color
        self.promotion_callback = promotion_callback

    def is_own_piece(self, piece):
        return (self.color == "white" and piece.isupper()) or (self.color == "black" and piece.islower())

    def handle_click(self, board, selected, row, col, turn):
        piece = board.grid[row][col]
        legal_moves = []

        if selected:
            if (row, col) == selected:
                return None, [], turn  # Deselect
            elif piece != 'e' and self.is_own_piece(piece):
                return (row, col), get_legal_moves(board.grid, turn, row, col, board.last_move, board.rights), turn
            else:
                promotion = None
                # Check for promotion
                if board.grid[selected[0]][selected[1]].lower() == 'p' and (row == 0 or row == 7):
                    if self.promotion_callback:
                        promotion = self.promotion_callback(turn, row, col)

                board.grid, new_turn, board.last_move, board.rights, moved = move_piece(
                    board.grid, selected, (row, col), turn,
                    board.last_move, board.rights,
                    promotion_choice=promotion
                )
                if moved:
                    board.update_check_status()
                    if is_checkmate(board.grid, new_turn, board.last_move, board.rights):
                        print(f"Checkmate! {turn.capitalize()} wins.")
                    elif is_stalemate(board.grid, new_turn, board.last_move, board.rights):
                        print("Stalemate! It's a draw.")
                    return None, [], new_turn
                return None, [], turn
        else:
            if piece != 'e' and self.is_own_piece(piece):
                return (row, col), get_legal_moves(board.grid, turn, row, col, board.last_move, board.rights), turn
        return selected, legal_moves, turn
