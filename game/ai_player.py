from logic.prolog_interface import get_minimax_move, move_piece, is_checkmate, is_stalemate, get_board_hash
import time
import multiprocessing

class AIMinimaxProcess:
    def __init__(self, board, color, depth, last_move, rights, state_history):
        self.queue = multiprocessing.Queue()
        self.process = multiprocessing.Process(
            target=self._worker,
            args=(self.queue, board, color, depth, last_move, rights, state_history)
        )
        self.result = None

    def _worker(self, queue, board, color, depth, last_move, rights, state_history):
        try:
            result = get_minimax_move(board, color, depth, last_move, rights, state_history)
            queue.put(result)
        except Exception as e:
            queue.put(None)

    def start(self):
        self.process.start()

    def get_result(self):
        if self.process.is_alive():
            return None
        if not self.queue.empty():
            self.result = self.queue.get()
        self.process.join()
        return self.result

    def terminate(self):
        if self.process.is_alive():
            self.process.terminate()
            self.process.join()

    def is_alive(self):
        return self.process.is_alive()


class AIPlayer:
    def __init__(self, color, board):
        self.color = color
        self.board = board
        self.ai_task = None
        self.ai_start_time = None
        self.opponent_color = 'black' if color == 'white' else 'white'

    def terminate_task(self):
        if (self.ai_task):
            self.ai_task.terminate()

    def write_history(self, promotion=None, special=None):
        last_move = self.board.last_move
        if promotion:
            last_move = last_move + (promotion,)
        if special:
            last_move = last_move + (special,)
        self.board.history.append(last_move)
        self.board.state_history.append(
            get_board_hash(self.board.grid, self.opponent_color, self.board.last_move, self.board.rights)
        )

    def get_dynamic_depth(self):
        flat = sum(self.board.grid, [])
        piece_count = sum(1 for p in flat if p not in ('e', ' '))
        if piece_count > 20:
            return 3
        elif 10 < piece_count <= 20:
            return 4
        else:
            return 5

    def make_move(self):
        if self.ai_task is None:
            depth = self.get_dynamic_depth()
            print(f"AI ({self.color}) using depth {depth}")
            self.ai_start_time = time.time()  # Lưu thời gian bắt đầu tính
            self.ai_task = AIMinimaxProcess(
                self.board.grid, self.color, depth,
                self.board.last_move, self.board.rights,
                self.board.state_history
            )
            self.ai_task.start()
            return self.color, True  # AI mới bắt đầu tính, chưa xong
        else:
            move_data = self.ai_task.get_result()
            if move_data is None:
                # AI đang tính, chưa xong
                return self.color, True

            # AI đã tính xong
            elapsed = time.time() - self.ai_start_time
            print(f"AI ({self.color}) computed move in {elapsed:.3f} seconds.")
            self.ai_task = None
            self.ai_start_time = None

            if not move_data or move_data['BestMove'] == 'none':
                print("AI has no legal moves.")
                return self.color, False

            # Parse best move
            move_args = move_data['BestMove'].replace('move(', '').replace(')', '').split(',')
            from_row, from_col, to_row, to_col = map(int, move_args[:4])
            promotion = move_args[4].strip()
            if promotion == 'none':
                promotion = None

            piece = self.board.grid[from_row][from_col]
            target = self.board.grid[to_row][to_col]
            special = None

            if piece.lower() == 'k' and abs(from_col - to_col) == 2:
                special = 'castle_kingside' if to_col == 6 else 'castle_queenside'
            if piece.lower() == 'p' and target == 'e' and from_col != to_col:
                special = 'en_passant'

            # Execute the move
            self.board.grid, new_turn, self.board.last_move, self.board.rights, moved = move_piece(
                self.board.grid,
                (from_row, from_col),
                (to_row, to_col),
                self.color,
                self.board.last_move,
                self.board.rights,
                promotion_choice=promotion
            )

            if moved:
                self.board.update_check_status()
                self.write_history(promotion=promotion, special=special)

                if is_checkmate(self.board.grid, new_turn, self.board.last_move, self.board.rights):
                    print(f"Checkmate! {self.color.capitalize()} wins.")
                    self.board.history.append([f"Checkmate! {self.color.capitalize()} wins."])
                    return new_turn, False
                elif is_stalemate(self.board.grid, new_turn, self.board.last_move, self.board.rights):
                    print("Stalemate! It's a draw.")
                    self.board.history.append(["Stalemate! It's a draw."])
                    return new_turn, False

                return new_turn, True
            else:
                print("AI move failed.")
                return self.color, False
