from pyswip.prolog import Prolog

prolog = Prolog()

prolog.consult('utils.pl')
prolog.consult('piece_rule.pl')

def python_board_to_prolog(board):
    return "[" + ",".join("[" + ",".join(f"'{p}'" for p in row) + "]" for row in board) + "]"

def get_legal_moves(board, color, r, c):
    board_str = python_board_to_prolog(board)
    query = f"legal_move({board_str}, {color}, {r}, {c}, ToR, ToC)"
    return [(res["ToR"], res["ToC"]) for res in prolog.query(query)]

def move_piece(board, start, end, turn):
    sr, sc = start
    er, ec = end
    piece = board[sr][sc]

    if (turn == "white" and piece.isupper()) or (turn == "black" and piece.islower()):
        board_str = python_board_to_prolog(board)
        query = f"legal_move({board_str}, {turn}, {sr}, {sc}, {er}, {ec})"
        if list(prolog.query(query)):
            board[er][ec] = piece
            board[sr][sc] = 'e'
            turn = "black" if turn == "white" else "white"
            return board, turn, True
    return board, turn, False
