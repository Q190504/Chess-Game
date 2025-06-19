from logic.prolog_interface import get_legal_moves, move_piece, is_checkmate, is_stalemate

class Player:
    def __init__(self, color, promotion_callback=None):
        self.color = color
        self.promotion_callback = promotion_callback

    def is_own_piece(self, piece):
        return (self.color == "white" and piece.isupper()) or (self.color == "black" and piece.islower())

    @staticmethod
    def write_history(board, last_move, promotion=None, special=None):
        if promotion:
            last_move = last_move + (promotion,)
        if special:
            last_move = last_move + (special,)
        board.history.append(last_move)

    def handle_click(self, board, selected, row, col, turn):
        if not (0 <= row < 8 and 0 <= col < 8):
            return None, [], turn  # Click is out of bounds

        piece = board.grid[row][col]
        legal_moves = []

        if selected:
            if (row, col) == selected:
                return None, [], turn  # Deselect

            elif piece != 'e' and self.is_own_piece(piece):
                # Select a different own piece
                return (row, col), get_legal_moves(board.grid, turn, row, col, board.last_move, board.rights), turn

            else:
                # Attempt move
                promotion = None
                special = None
                from_r, from_c = selected
                to_r, to_c = row, col
                piece = board.grid[from_r][from_c]
                target = board.grid[to_r][to_c]

                # Handle promotion
                if piece.lower() == 'p' and (to_r == 0 or to_r == 7):
                    if self.promotion_callback:
                        promotion = self.promotion_callback(turn, to_r, to_c)

                # Detect castling
                if piece.lower() == 'k' and abs(from_c - to_c) == 2:
                    if to_c == 6:
                        special = "castle_kingside"
                    elif to_c == 2:
                        special = "castle_queenside"

                # Detect en passant
                if piece.lower() == 'p' and target == 'e' and from_c != to_c and board.grid[to_r][to_c] == 'e':
                    special = "en_passant"

                board.grid, new_turn, board.last_move, board.rights, moved = move_piece(
                    board.grid, selected, (row, col), turn,
                    board.last_move, board.rights, promotion_choice=promotion
                )

                if moved:
                    board.update_check_status()
                    self.write_history(board, board.last_move, promotion=promotion, special=special)
                    print(f"Player moved {board.grid[row][col]} from {selected} to ({row}, {col})")

                    if is_checkmate(board.grid, new_turn, board.last_move, board.rights):
                        print(f"Checkmate! {turn.capitalize()} wins.")
                    elif is_stalemate(board.grid, new_turn, board.last_move, board.rights):
                        print("Stalemate! It's a draw.")

                    return None, [], new_turn
                return None, [], turn

        else:
            # No selection yet — select own piece
            if piece != 'e' and self.is_own_piece(piece):
                return (row, col), get_legal_moves(board.grid, turn, row, col, board.last_move, board.rights), turn

        return selected, legal_moves, turn
