from pyswip.prolog import Prolog

prolog = Prolog()

prolog.consult('utils.pl')
prolog.consult('piece_rule.pl')

def python_board_to_prolog(board):
    return "[" + ",".join("[" + ",".join(f"'{p}'" for p in row) + "]" for row in board) + "]"

def get_legal_moves(board, color, r, c, last_move=None):
    board_str = python_board_to_prolog(board)
    
    if last_move:
        from_r, from_c, to_r, to_c, piece = last_move
        move_term = f"move({from_r},{from_c},{to_r},{to_c},'{piece}')"
    else:
        move_term = "none"
        
    query = f"legal_move({board_str}, {color}, {r}, {c}, ToR, ToC, {move_term})"
    return [(res["ToR"], res["ToC"]) for res in prolog.query(query)]

def move_piece(board, start, end, turn, last_move=None):
    sr, sc = start
    er, ec = end
    piece = board[sr][sc]
    if (turn == "white" and piece.isupper()) or (turn == "black" and piece.islower()):
        board_str = python_board_to_prolog(board)

        if last_move:
            from_r, from_c, to_r, to_c, old_piece = last_move
            move_term = f"move({from_r},{from_c},{to_r},{to_c},'{old_piece}')"
        else:
            move_term = "none"
        
        query = f"safe_move({board_str}, {turn}, {sr}, {sc}, {er}, {ec}, {move_term})"
        if list(prolog.query(query)):
            is_en_passant = False
            en_passant_query = f"en_passant({board_str}, {turn}, {sr}, {sc}, {er}, {ec}, {move_term})"
            is_en_passant = bool(list(prolog.query(en_passant_query)))
            
            board[er][ec] = piece
            board[sr][sc] = 'e'
            if is_en_passant:
                if turn == "white":
                    board[er + 1][ec] = 'e'
                else:
                    board[er - 1][ec] = 'e'
                    
            next_turn = "black" if turn == "white" else "white"
 
            new_last_move = (sr, sc, er, ec, piece)
            return board, next_turn, new_last_move, True
            
    return board, turn, last_move, False

def is_in_check(board, turn):
    board_str = python_board_to_prolog(board)
    query = f"in_check({board_str}, {turn})"
    return bool(list(prolog.query(query)))

def is_checkmate(board, turn, last_move):
    board_str = python_board_to_prolog(board)
    query = f"checkmate({board_str}, {turn}, {last_move})"
    return bool(list(prolog.query(query)))

def is_stalemate(board, turn, last_move):
    board_str = python_board_to_prolog(board)
    query = f"stalemate({board_str}, {turn}, {last_move})"
    return bool(list(prolog.query(query)))