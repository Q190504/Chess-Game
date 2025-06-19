from logic.prolog_interface import get_minimax_move, move_piece, is_checkmate, is_stalemate

class AIPlayer:
    def __init__(self, color):
        self.color = color

    def make_move(self, board):
        move_data = get_minimax_move(board.grid, self.color, 3, board.last_move, board.rights)
        if not move_data:
            print("AI has no legal moves.")
            return self.color  # No move, turn stays the same

        best_move = move_data['BestMove']
        move_args = best_move.replace('move(', '').replace(')', '').split(',')
        from_row, from_col, to_row, to_col = map(int, move_args[:4])
        promotion = move_args[4].strip()
        if promotion == 'none':
            promotion = None

        # Execute the move
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
            if is_checkmate(board.grid, new_turn, board.last_move, board.rights):
                print(f"Checkmate! {self.color.capitalize()} wins.")
            elif is_stalemate(board.grid, new_turn, board.last_move, board.rights):
                print("Stalemate! It's a draw.")
            return new_turn
        else:
            print("AI move failed.")
            return self.color
