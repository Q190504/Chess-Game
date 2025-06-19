from pyswip.prolog import Prolog

prolog = Prolog()

prolog.consult('utils.pl')
prolog.consult('piece_rule.pl')

def python_board_to_prolog(board):
    return "[" + ",".join("[" + ",".join(f"'{p}'" for p in row) + "]" for row in board) + "]"

def get_move_term(move):
    if move:
        from_r, from_c, to_r, to_c, old_piece = move
        move_term = f"move({from_r},{from_c},{to_r},{to_c},'{old_piece}')"
    else:
        move_term = "none"
    return move_term

def get_castle_rights_term(rights):
    if (rights):
        wk = str(rights["white_kingside"]).lower()
        wq = str(rights["white_queenside"]).lower()
        bk = str(rights["black_kingside"]).lower()
        bq = str(rights["black_queenside"]).lower()
        term = f"castle_rights({wk},{wq},{bk},{bq})"
    else:
        term = "None"
    return term

def get_legal_moves(board, color, r, c, last_move=None, castle_rights=None):
    board_str = python_board_to_prolog(board)
    move_term = get_move_term(last_move)
    castle_rights_terms = get_castle_rights_term(castle_rights)
    query = f"legal_move({board_str}, {color}, {r}, {c}, ToR, ToC, {move_term}, {castle_rights_terms})"
    return [(res["ToR"], res["ToC"]) for res in prolog.query(query)]

def move_piece(board, start, end, turn, last_move=None, castle_rights=None):
    sr, sc = start
    er, ec = end
    piece = board[sr][sc]
    if (turn == "white" and piece.isupper()) or (turn == "black" and piece.islower()):
        board_str = python_board_to_prolog(board)
        move_term = get_move_term(last_move)
        castle_rights_terms = get_castle_rights_term(castle_rights)

        query = f"safe_move({board_str}, {turn}, {sr}, {sc}, {er}, {ec}, {move_term}, {castle_rights_terms})"
        if list(prolog.query(query)):
            is_en_passant = False
            if piece in ['P', 'p']:
                en_passant_query = f"en_passant({board_str}, {turn}, {sr}, {sc}, {er}, {ec}, {move_term})"
                is_en_passant = bool(list(prolog.query(en_passant_query)))
            
            board[er][ec] = piece
            board[sr][sc] = 'e'
            if is_en_passant:
                if turn == "white":
                    board[er + 1][ec] = 'e'
                else:
                    board[er - 1][ec] = 'e'

            if piece in ['K', 'k']:
                if turn == "white" and sr == 7 and sc == 4:
                    if (er, ec) == (7, 6):  # kingside
                        board[7][5] = 'R'
                        board[7][7] = 'e'

                    elif (er, ec) == (7, 2):  # queenside
                        board[7][3] = 'R'
                        board[7][0] = 'e'

                elif turn == "black" and sr == 0 and sc == 4:
                    if (er, ec) == (0, 6):  # kingside
                        board[0][5] = 'r'
                        board[0][7] = 'e'

                    elif (er, ec) == (0, 2):  # queenside
                        board[0][3] = 'r'
                        board[0][0] = 'e'

            new_rights = castle_rights.copy()
            if piece in ['K', 'k']:
                if turn == "white":
                    new_rights["white_kingside"] = False
                    new_rights["white_queenside"] = False
                else:
                    new_rights["black_kingside"] = False
                    new_rights["black_queenside"] = False

            if piece in ['R', 'r']:
                # Check if rook moved from original corner
                if turn == "white":
                    if sr == 7 and sc == 0:
                        new_rights["white_queenside"] = False
                    elif sr == 7 and sc == 7:
                        new_rights["white_kingside"] = False
                else:
                    if sr == 0 and sc == 0:
                        new_rights["black_queenside"] = False
                    elif sr == 0 and sc == 7:
                        new_rights["black_kingside"] = False

            next_turn = "black" if turn == "white" else "white"
 
            new_last_move = (sr, sc, er, ec, piece)
            return board, next_turn, new_last_move, new_rights, True
            
    return board, turn, last_move, castle_rights, False

def is_in_check(board, turn):
    board_str = python_board_to_prolog(board)
    query = f"in_check({board_str}, {turn})"
    return bool(list(prolog.query(query)))

def is_checkmate(board, turn, last_move=None, castle_rights=None):
    board_str = python_board_to_prolog(board)
    move_term = get_move_term(last_move)
    castle_rights_terms = get_castle_rights_term(castle_rights)
    query = f"checkmate({board_str}, {turn}, {move_term}, {castle_rights})"
    return bool(list(prolog.query(query)))

def is_stalemate(board, turn, last_move=None, castle_rights=None):
    board_str = python_board_to_prolog(board)
    move_term = get_move_term(last_move)
    castle_rights_terms = get_castle_rights_term(castle_rights)
    query = f"stalemate({board_str}, {turn}, {move_term}, {castle_rights})"
    return bool(list(prolog.query(query)))

