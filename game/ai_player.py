from logic.prolog_interface import get_minimax_move, move_piece, is_checkmate, is_stalemate
import time 

class AIPlayer:
    def __init__(self, color):
        self.color = color

    @staticmethod
    def write_history(board, last_move, promotion=None, special=None):
        if promotion:
            last_move = last_move + (promotion,)
        if special:
            last_move = last_move + (special,)
        board.history.append(last_move)

    def make_move(self, board):
        start_time = time.time()  # bắt đầu tính giờ

        move_data = get_minimax_move(board.grid, self.color, 3, board.last_move, board.rights)

        end_time = time.time()  # kết thúc tính giờ
        elapsed = end_time - start_time
        print(f"AI ({self.color}) computed move in {elapsed:.3f} seconds.")

        if not move_data:
            print("AI has no legal moves.")
            return self.color, False  # No move, turn stays the same
        
        best_move = move_data['BestMove']
        if best_move == 'none':
            print("AI has no legal moves.")
            return self.color, False  # No move, turn stays the same
        
        move_args = best_move.replace('move(', '').replace(')', '').split(',')
        from_row, from_col, to_row, to_col = map(int, move_args[:4])
        promotion = move_args[4].strip()
        if promotion == 'none':
            promotion = None

        # Detect special moves before applying
        piece = board.grid[from_row][from_col]
        target = board.grid[to_row][to_col]
        special = None

        # Castling
        if piece.lower() == 'k' and abs(from_col - to_col) == 2:
            if to_col == 6:
                special = 'castle_kingside'
            elif to_col == 2:
                special = 'castle_queenside'

        # En passant
        if piece.lower() == 'p' and target == 'e' and from_col != to_col and board.grid[to_row][to_col] == 'e':
            special = 'en_passant'

        # Move piece
        board.grid, new_turn, board.last_move, board.rights, moved = move_piece(
            board.grid,
            (from_row, from_col),
            (to_row, to_col),
            self.color,
            board.last_move,
            board.rights,
            promotion_choice=promotion
        )

        if moved:
            board.update_check_status()
            self.write_history(board, board.last_move, promotion=promotion, special=special)

            if is_checkmate(board.grid, new_turn, board.last_move, board.rights):
                print(f"Checkmate! {self.color.capitalize()} wins.")
                board.history.append([f"Checkmate! {self.color.capitalize()} wins."])
                return new_turn, False
            elif is_stalemate(board.grid, new_turn, board.last_move, board.rights):
                print("Stalemate! It's a draw.")
                board.history.append(["Stalemate! It's a draw."])
                return new_turn, False
            return new_turn, True
        else:
            print("AI move failed.")
            return self.color, False
