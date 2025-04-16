% ------------------------------
% Pawn
% A white pawn can move one step forward to an empty square or two steps forward from its starting position (row 6).
% It can also capture an opponent piece diagonally to the left or right.
% ------------------------------

% white pawn: one step
pawn_move(Board, white, FromRow, Col, ToRow, Col) :-
    ToRow is FromRow - 1,
    ToRow >= 0,
    get_piece(Board, ToRow, Col, e).

% white pawn: two steps from starting row (row 6)
pawn_move(Board, white, 6, Col, 4, Col) :-
    get_piece(Board, 5, Col, e),
    get_piece(Board, 4, Col, e).

% white pawn: diagonal capture
pawn_move(Board, white, FromRow, FromCol, ToRow, ToCol) :-
    ToRow is FromRow - 1,
    ToRow >= 0,
    (ToCol is FromCol - 1 ; ToCol is FromCol + 1),
    ToCol >= 0, ToCol =< 7,
    get_piece(Board, ToRow, ToCol, Piece),
    Piece \= e,
    is_black(Piece).


% black pawn: one step
pawn_move(Board, black, FromRow, Col, ToRow, Col) :-
    ToRow is FromRow + 1,
    ToRow =< 7,
    get_piece(Board, ToRow, Col, e).

% black pawn: two steps from starting row (row 1)
pawn_move(Board, black, 1, Col, 3, Col) :-
    get_piece(Board, 2, Col, e),
    get_piece(Board, 3, Col, e).

% black pawn: diagonal capture
pawn_move(Board, black, FromRow, FromCol, ToRow, ToCol) :-
    ToRow is FromRow + 1,
    ToRow =< 7,
    (ToCol is FromCol - 1 ; ToCol is FromCol + 1),
    ToCol >= 0, ToCol =< 7,
    get_piece(Board, ToRow, ToCol, Piece),
    Piece \= e,
    is_white(Piece).


% ------------------------------
% Knight
% A knight can move in an L-shape: two squares in one direction and then one square perpendicular to that direction.
% It can jump over other pieces, so it doesnt matter if the squares in between are occupied.
% ------------------------------

% All 8 L-shaped jumps
% knight_offset(Dr, Dc) means the knight can move Dr rows and Dc columns.
knight_offset(2, 1). knight_offset(1, 2).
knight_offset(-1, 2). knight_offset(-2, 1).
knight_offset(-2, -1). knight_offset(-1, -2).
knight_offset(1, -2). knight_offset(2, -1).

knight_move(Board, Color, FromRow, FromCol, ToRow, ToCol) :-
    knight_offset(Dr, Dc),
    ToRow is FromRow + Dr,
    ToCol is FromCol + Dc,
    exist_sqr(ToRow, ToCol),
    get_piece(Board, ToRow, ToCol, Target),
    (Target = e ; opponent(Color, Target)).


% ------------------------------
% Rook
% A rook can move any number of squares along a row or column
% Cannot jump over other pieces.
% ------------------------------

% Rook directions: vertical and horizontal
rook_dir(1, 0). rook_dir(-1, 0). rook_dir(0, 1). rook_dir(0, -1).

rook_move(Board, Color, FromRow, FromCol, ToRow, ToCol) :-
    rook_dir(Dr, Dc),
    slide(Board, Color, FromRow, FromCol, Dr, Dc, ToRow, ToCol).


% ------------------------------
% Bishop
% A bishop can move any number of squares diagonally.
% Cannot jump over other pieces.
% ------------------------------

% Bishop directions
bishop_dir(1, 1). bishop_dir(1, -1).
bishop_dir(-1, 1). bishop_dir(-1, -1).

bishop_move(Board, Color, FromRow, FromCol, ToRow, ToCol) :-
    bishop_dir(Dr, Dc),
    slide(Board, Color, FromRow, FromCol, Dr, Dc, ToRow, ToCol).


% ------------------------------
% Queen
% A queen can move any number of squares along a row, column, or diagonal (Combine rook + bishop).
% Cannot jump over other pieces.
% ------------------------------

% Combine rook + bishop directions
queen_dir(1, 0). queen_dir(-1, 0). queen_dir(0, 1). queen_dir(0, -1).
queen_dir(1, 1). queen_dir(1, -1). queen_dir(-1, 1). queen_dir(-1, -1).

queen_move(Board, Color, FromRow, FromCol, ToRow, ToCol) :-
    queen_dir(Dr, Dc),
    slide(Board, Color, FromRow, FromCol, Dr, Dc, ToRow, ToCol).


% ------------------------------
% King
% A king can move one square in any direction (horizontally, vertically, or diagonally).
% It cannot move into check (if the square is attacked by an opponent piece).
% ------------------------------

% King directions
king_offset(1, 0). king_offset(-1, 0).
king_offset(0, 1). king_offset(0, -1).
king_offset(1, 1). king_offset(1, -1).
king_offset(-1, 1). king_offset(-1, -1).

% King Move Logic
king_move(Board, Color, FromRow, FromCol, ToRow, ToCol) :-
    king_offset(Dr, Dc),
    ToRow is FromRow + Dr,
    ToCol is FromCol + Dc,
    exist_sqr(ToRow, ToCol),
    get_piece(Board, ToRow, ToCol, Target),
    (Target = e ; opponent(Color, Target)).