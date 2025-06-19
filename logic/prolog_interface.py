from pyswip.prolog import Prolog

prolog = Prolog()
prolog.consult('utils.pl')
prolog.consult('piece_rule.pl')
prolog.consult('piece_bonus.pl')
prolog.consult('positional_bonus.pl')
prolog.consult('minimax.pl')

# -------------------- Utility Conversion Functions --------------------

def python_board_to_prolog(board):
    return "[" + ",".join("[" + ",".join(f"'{p}'" for p in row) + "]" for row in board) + "]"


def get_move_term(move):
    if move:
        from_r, from_c, to_r, to_c, old_piece = move
        return f"move({from_r},{from_c},{to_r},{to_c},'{old_piece}')"
    return "none"


def get_castle_rights_term(rights):
    if rights:
        return f"castle_rights({str(rights['white_kingside']).lower()},{str(rights['white_queenside']).lower()}," \
               f"{str(rights['black_kingside']).lower()},{str(rights['black_queenside']).lower()})"
    return "None"


# -------------------- Game State Checks --------------------

def is_in_check(board, turn):
    board_str = python_board_to_prolog(board)
    return bool(list(prolog.query(f"in_check({board_str}, {turn})")))


def is_checkmate(board, turn, last_move=None, castle_rights=None):
    board_str = python_board_to_prolog(board)
    move_term = get_move_term(last_move)
    rights_term = get_castle_rights_term(castle_rights)
    return bool(list(prolog.query(f"checkmate({board_str}, {turn}, {move_term}, {rights_term})")))


def is_stalemate(board, turn, last_move=None, castle_rights=None):
    board_str = python_board_to_prolog(board)
    move_term = get_move_term(last_move)
    rights_term = get_castle_rights_term(castle_rights)
    return bool(list(prolog.query(f"stalemate({board_str}, {turn}, {move_term}, {rights_term})")))


# -------------------- Move Query --------------------

def get_legal_moves(board, color, r, c, last_move=None, castle_rights=None):
    board_str = python_board_to_prolog(board)
    move_term = get_move_term(last_move)
    rights_term = get_castle_rights_term(castle_rights)
    query = f"legal_move({board_str}, {color}, {r}, {c}, ToR, ToC, {move_term}, {rights_term})"
    return [(res["ToR"], res["ToC"]) for res in prolog.query(query)]


# -------------------- Sub Move Handlers --------------------

def handle_en_passant(board, piece, sr, sc, er, ec, turn, board_str, last_move):
    if piece.lower() == 'p':
        move_term = get_move_term(last_move)
        query = f"en_passant({board_str}, {turn}, {sr}, {sc}, {er}, {ec}, {move_term})"
        if list(prolog.query(query)):
            capture_row = er + 1 if turn == "white" else er - 1
            board[capture_row][ec] = 'e'
            return True
    return False


def handle_castling(board, piece, sr, sc, er, ec, turn):
    if piece.lower() == 'k':
        if turn == "white" and (sr, sc) == (7, 4):
            if (er, ec) == (7, 6):  # Kingside
                board[7][5], board[7][7] = 'R', 'e'
            elif (er, ec) == (7, 2):  # Queenside
                board[7][3], board[7][0] = 'R', 'e'
        elif turn == "black" and (sr, sc) == (0, 4):
            if (er, ec) == (0, 6):  # Kingside
                board[0][5], board[0][7] = 'r', 'e'
            elif (er, ec) == (0, 2):  # Queenside
                board[0][3], board[0][0] = 'r', 'e'

def handle_promotion(board, piece, er, ec, turn, promotion_choice=None):
    if piece.lower() == 'p':
        if (turn == "white" and er == 0) or (turn == "black" and er == 7):
            if promotion_choice is None:
                promotion_choice = 'Q' if turn == "white" else 'q'
            else:
                promotion_choice = promotion_choice.upper() if turn == "white" else promotion_choice.lower()
            board[er][ec] = promotion_choice


def update_castle_rights(rights, piece, sr, sc, turn):
    new_rights = rights.copy() if rights else {}
    if piece.lower() == 'k':
        if turn == "white":
            new_rights["white_kingside"] = False
            new_rights["white_queenside"] = False
        else:
            new_rights["black_kingside"] = False
            new_rights["black_queenside"] = False
    elif piece.lower() == 'r':
        if turn == "white":
            if (sr, sc) == (7, 0): new_rights["white_queenside"] = False
            if (sr, sc) == (7, 7): new_rights["white_kingside"] = False
        else:
            if (sr, sc) == (0, 0): new_rights["black_queenside"] = False
            if (sr, sc) == (0, 7): new_rights["black_kingside"] = False
    return new_rights


# -------------------- Main Move Function --------------------

def move_piece(board, start, end, turn, last_move=None, castle_rights=None, promotion_choice = None):
    sr, sc = start
    er, ec = end
    piece = board[sr][sc]

    if (turn == "white" and piece.isupper()) or (turn == "black" and piece.islower()):
        board_str = python_board_to_prolog(board)
        move_term = get_move_term(last_move)
        rights_term = get_castle_rights_term(castle_rights)

        query = f"safe_move({board_str}, {turn}, {sr}, {sc}, {er}, {ec}, {move_term}, {rights_term})"
        if list(prolog.query(query)):
            # Perform move
            board[er][ec], board[sr][sc] = piece, 'e'

            handle_en_passant(board, piece, sr, sc, er, ec, turn, board_str, last_move)

            handle_castling(board, piece, sr, sc, er, ec, turn)

            handle_promotion(board, piece, er, ec, turn, promotion_choice) 

            new_rights = update_castle_rights(castle_rights, piece, sr, sc, turn)

            # Prepare for next turn
            next_turn = "black" if turn == "white" else "white"
            new_last_move = (sr, sc, er, ec, piece)
            
            return board, next_turn, new_last_move, new_rights, True

    return board, turn, last_move, castle_rights, False

def get_minimax_move(board, turn, depth = 3, last_move=None, castle_rights=None):
    board_str = python_board_to_prolog(board)
    move_term = get_move_term(last_move)
    rights_term = get_castle_rights_term(castle_rights)
    
    result = list(prolog.query(f"minimax({board_str}, {turn}, {depth}, {move_term}, {rights_term}, BestMove, Value)."))
    
    print(result)
    if len(result) > 0:
        return result[0]
    return None