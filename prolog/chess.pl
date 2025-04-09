% Define the board with initial positions
piece(rook, white, a1).
piece(knight, white, b1).
piece(bishop, white, c1).
piece(queen, white, d1).
piece(king, white, e1).
piece(bishop, white, f1).
piece(knight, white, g1).
piece(rook, white, h1).
piece(pawn, white, a2).
piece(pawn, white, b2).
piece(pawn, white, c2).
piece(pawn, white, d2).
piece(pawn, white, e2).
piece(pawn, white, f2).
piece(pawn, white, g2).
piece(pawn, white, h2).

% Rules for movement (Example: Pawn)
valid_move(pawn, white, X, Y, X, Y2) :- Y2 is Y + 1.
valid_move(pawn, black, X, Y, X, Y2) :- Y2 is Y - 1.

% Rules for capturing diagonally
valid_move(pawn, white, X, Y, X2, Y2) :- X2 is X + 1, Y2 is Y + 1.
valid_move(pawn, white, X, Y, X2, Y2) :- X2 is X - 1, Y2 is Y + 1.

% Rule for check (Example: King is threatened)
in_check(Color) :-
    piece(king, Color, X, Y),
    piece(Piece, OpponentColor, X2, Y2),
    valid_move(Piece, OpponentColor, X2, Y2, X, Y).
