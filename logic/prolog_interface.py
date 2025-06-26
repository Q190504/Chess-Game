from pyswip.prolog import Prolog

prolog = Prolog()
prolog.consult('utils.pl')
prolog.consult('piece_rule.pl')
prolog.consult('piece_bonus.pl')
prolog.consult('positional_bonus.pl')
prolog.consult('move_generation.pl')
prolog.consult('minimax.pl')

# -------------------- Utility Conversion Functions --------------------
def python_list_to_prolog_list(py_list):
    return "[" + ",".join(str(x) for x in py_list) + "]"

def python_board_to_prolog(board):
    return "[" + ",".join("[" + ",".join(f"'{p}'" for p in row) + "]" for row in board) + "]"

def get_board_hash(board, color, last_move, castle_rights):
    board_str = python_board_to_prolog(board)
    move_term = get_move_term(last_move)
    rights_term = get_castle_rights_term(castle_rights)
    
    query = f"rep_board_hash({board_str}, {color}, {move_term}, {rights_term}, Hash)"
    q = prolog.query(query)
    result = list(q)
    q.close()
    if result:
        return result[0]["Hash"]
    return None

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
    query = f"in_check({board_str}, {turn})"
    q = prolog.query(query)
    result = list(q)
    q.close()
    return bool(result)


def is_checkmate(board, turn, last_move=None, castle_rights=None):
    board_str = python_board_to_prolog(board)
    move_term = get_move_term(last_move)
    rights_term = get_castle_rights_term(castle_rights)
    query = f"checkmate({board_str}, {turn}, {move_term}, {rights_term})"
    q = prolog.query(query)
    result = list(q)
    q.close()
    return bool(result)

def is_stalemate(board, turn, last_move=None, castle_rights=None):
    board_str = python_board_to_prolog(board)
    move_term = get_move_term(last_move)
    rights_term = get_castle_rights_term(castle_rights)
    query = f"stalemate({board_str}, {turn}, {move_term}, {rights_term})"
    q = prolog.query(query)
    result = list(q)
    q.close()
    return bool(result)


# -------------------- Move Query --------------------

def get_legal_moves(board, color, r, c, last_move=None, castle_rights=None):
    board_str = python_board_to_prolog(board)
    move_term = get_move_term(last_move)
    rights_term = get_castle_rights_term(castle_rights)
    query = f"legal_move({board_str}, {color}, {r}, {c}, ToR, ToC, {move_term}, {rights_term})"
    q = prolog.query(query)
    result = list(q)
    q.close()
    return [(res["ToR"], res["ToC"]) for res in result]

# -------------------- Sub Move Handlers --------------------

def handle_en_passant(board, piece, sr, sc, er, ec, turn, board_str, last_move):
    if piece.lower() == 'p':
        move_term = get_move_term(last_move)
        query = f"en_passant({board_str}, {turn}, {sr}, {sc}, {er}, {ec}, {move_term})"
        q = prolog.query(query)
        result = list(q)
        q.close()
        if result:
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


def update_castle_rights(board, rights, piece, turn):
    new_rights = rights.copy() if rights else {}

    # King moved
    if piece.lower() == 'k':
        if turn == "white":
            new_rights["white_kingside"] = False
            new_rights["white_queenside"] = False
        else:
            new_rights["black_kingside"] = False
            new_rights["black_queenside"] = False

    # WHITE
    if board[7][0] != 'R':
        new_rights["white_queenside"] = False
    if board[7][7] != 'R':
        new_rights["white_kingside"] = False

    # BLACK
    if board[0][0] != 'r':
        new_rights["black_queenside"] = False
    if board[0][7] != 'r':
        new_rights["black_kingside"] = False

    return new_rights


# -------------------- Main Move Function --------------------

def move_piece(board, start, end, turn, last_move=None, castle_rights=None, promotion_choice=None):
    sr, sc = start
    er, ec = end
    piece = board[sr][sc]

    if (turn == "white" and piece.isupper()) or (turn == "black" and piece.islower()):
        board_str = python_board_to_prolog(board)
        move_term = get_move_term(last_move)
        rights_term = get_castle_rights_term(castle_rights)

        query = f"safe_move({board_str}, {turn}, {sr}, {sc}, {er}, {ec}, {move_term}, {rights_term})"
        q = prolog.query(query)
        result = list(q)
        q.close()
        if (result):
            # Perform move
            captured_piece = board[er][ec]
            board[er][ec], board[sr][sc] = piece, 'e'
            handle_en_passant(board, piece, sr, sc, er, ec, turn, board_str, last_move)
            handle_castling(board, piece, sr, sc, er, ec, turn)
            handle_promotion(board, piece, er, ec, turn, promotion_choice)
            new_rights = update_castle_rights(board, castle_rights, piece, turn)
            next_turn = "black" if turn == "white" else "white"
            new_last_move = (sr, sc, er, ec, piece)
            return board, next_turn, new_last_move, new_rights, True

    return board, turn, last_move, castle_rights, False


def get_minimax_move(board, turn, depth=3, last_move=None, castle_rights=None, history=None):
    if history is None:
        current_hash = get_board_hash(board, turn, last_move, castle_rights)
        history = [current_hash] if current_hash is not None else []

    board_str = python_board_to_prolog(board)
    move_term = get_move_term(last_move)
    rights_term = get_castle_rights_term(castle_rights)
    history_term = python_list_to_prolog_list(history)

    query = f"minimax({board_str}, {turn}, {depth}, {move_term}, {rights_term}, {history_term}, BestMove, Value)."
    q = prolog.query(query)
    result = list(q)
    q.close()

    print(result)
    if result:
        return result[0]
    return None


def shutdown_prolog():
    try:
        q = prolog.query("halt.")
        result = list(q)
        q.close()
    except Exception as e:
        print(f"Error shutting down Prolog: {e}")