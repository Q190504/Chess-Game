from logic.prolog_interface import get_legal_moves, move_piece, is_checkmate, is_stalemate, get_board_hash

class Player:
    def __init__(self, color, board, promotion_callback=None):
        self.color = color
        self.board = board
        self.opponent_color = 'black' if color == 'white' else 'white'
        self.promotion_callback = promotion_callback

    def is_own_piece(self, piece):
        return (self.color == "white" and piece.isupper()) or (self.color == "black" and piece.islower())

    def write_history(self, promotion=None, special=None):
        last_move = self.board.last_move
        if promotion:
            last_move = last_move + (promotion,)
        if special:
            last_move = last_move + (special,)
        self.board.history.append(last_move)
        self.board.state_history.append(get_board_hash(self.board.grid, self.opponent_color, self.board.last_move, self.board.rights))

    def handle_click(self, selected, row, col, turn):
        if not (0 <= row < 8 and 0 <= col < 8):
            return None, [], turn, True  # Click is out of bounds

        piece = self.board.grid[row][col]
        legal_moves = []

        if selected:
            if (row, col) == selected:
                return None, [], turn, True  # Deselect

            elif piece != 'e' and self.is_own_piece(piece):
                # Select a different own piece
                return (row, col), get_legal_moves(self.board.grid, turn, row, col, self.board.last_move, self.board.rights), turn, True

            else:
                # Attempt move
                promotion = None
                special = None
                from_r, from_c = selected
                to_r, to_c = row, col
                piece = self.board.grid[from_r][from_c]
                target = self.board.grid[to_r][to_c]

                # Handle promotion
                if piece.lower() == 'p' and (to_r == 0 or to_r == 7):
                    if (to_r, to_c) in get_legal_moves(self.board.grid, turn, from_r, from_c, self.board.last_move, self.board.rights):
                        if self.promotion_callback:
                            promotion = self.promotion_callback(turn, to_r, to_c)

                # Detect castling
                if piece.lower() == 'k' and abs(from_c - to_c) == 2:
                    if to_c == 6:
                        special = "castle_kingside"
                    elif to_c == 2:
                        special = "castle_queenside"

                # Detect en passant
                if piece.lower() == 'p' and target == 'e' and from_c != to_c and self.board.grid[to_r][to_c] == 'e':
                    special = "en_passant"

                self.board.grid, new_turn, self.board.last_move, self.board.rights, moved = move_piece(
                    self.board.grid, selected, (row, col), turn,
                    self.board.last_move, self.board.rights, promotion_choice=promotion
                )

                if moved:
                    self.board.update_check_status()
                    self.write_history(promotion=promotion, special=special)
                    print(f"Player moved {self.board.grid[row][col]} from {selected} to ({row}, {col})")

                    if is_checkmate(self.board.grid, new_turn, self.board.last_move, self.board.rights):
                        print(f"Checkmate! {turn.capitalize()} wins.")
                        self.board.history.append([f"Checkmate! {self.color.capitalize()} wins."])
                        return None, [], new_turn, False
                    elif is_stalemate(self.board.grid, new_turn, self.board.last_move, self.board.rights):
                        print("Stalemate! It's a draw.")
                        self.board.history.append(["Stalemate! It's a draw."])
                        return None, [], new_turn, False

                    return None, [], new_turn, True
                return None, [], turn, True

        else:
            # No selection yet — select own piece
            if piece != 'e' and self.is_own_piece(piece):
                return (row, col), get_legal_moves(self.board.grid, turn, row, col, self.board.last_move, self.board.rights), turn, True
        
        return selected, legal_moves, turn, True
