% ------------------------------
% Helpers
% ------------------------------

% Checks if a square is empty
is_empty(e).

% Check that a square is on the 8x8 board
exist_sqr(Row, Col) :-
    Row >= 0, Row =< 7,
    Col >= 0, Col =< 7.

% Check if the piece is an opponent piece
opponent(white, P) :- is_black(P).
opponent(black, P) :- is_white(P).

% Sliding Logic (shared by rook/bishop/queen)
% Slide in a direction (Dr, Dc) until hitting a piece or going out of bounds
% If the square is empty, keep sliding. If it hits an opponent piece, capture it then stop. If it hits a friendly piece, stop.
slide(Board, Color, Row, Col, Dr, Dc, ToRow, ToCol) :-
    NextRow is Row + Dr,
    NextCol is Col + Dc,
    exist_sqr(NextRow, NextCol),
    get_piece(Board, NextRow, NextCol, Target),
    (
        Target = e ->
            (ToRow = NextRow, ToCol = NextCol)
        ;
        opponent(Color, Target) ->
            (ToRow = NextRow, ToCol = NextCol)
        ;
        \+ opponent(Color, Target) ->
            fail
    ).

slide(Board, Color, Row, Col, Dr, Dc, ToRow, ToCol) :-
    NextRow is Row + Dr,
    NextCol is Col + Dc,
    exist_sqr(NextRow, NextCol),
    get_piece(Board, NextRow, NextCol, Target),
    Target = e,
    slide(Board, Color, NextRow, NextCol, Dr, Dc, ToRow, ToCol).


% Opponent Checker
opponent(white, P) :- is_black(P).
opponent(black, P) :- is_white(P).


% Legal Move Checker
% white
legal_move(Board, white, R, C, ToR, ToC) :-
    get_piece(Board, R, C, Piece),
    (
        Piece = 'P', pawn_move(Board, white, R, C, ToR, ToC)
    ;   Piece = 'N', knight_move(Board, white, R, C, ToR, ToC)
    ;   Piece = 'R', rook_move(Board, white, R, C, ToR, ToC)
    ;   Piece = 'B', bishop_move(Board, white, R, C, ToR, ToC)
    ;   Piece = 'Q', queen_move(Board, white, R, C, ToR, ToC)
    ;   Piece = 'K', king_move(Board, white, R, C, ToR, ToC)
    ).
% black
legal_move(Board, black, R, C, ToR, ToC) :-
    get_piece(Board, R, C, Piece),
    (
        Piece = 'p', pawn_move(Board, black, R, C, ToR, ToC)
    ;   Piece = 'n', knight_move(Board, black, R, C, ToR, ToC)
    ;   Piece = 'r', rook_move(Board, black, R, C, ToR, ToC)
    ;   Piece = 'b', bishop_move(Board, black, R, C, ToR, ToC)
    ;   Piece = 'q', queen_move(Board, black, R, C, ToR, ToC)
    ;   Piece = 'k', king_move(Board, black, R, C, ToR, ToC)
    ).


% White pieces
is_white('P').
is_white('R').
is_white('N').
is_white('B').
is_white('Q').
is_white('K').

% Black pieces 
is_black('p').
is_black('r').
is_black('n').
is_black('b').
is_black('q').
is_black('k').


% Gets a piece at (Row, Col) from the Board
get_piece(Board, Row, Col, Piece) :-
    nth0(Row, Board, BoardRow),
    nth0(Col, BoardRow, Piece).

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
legal_move(Board, black, FromRow, Col, ToRow, Col) :-
    ToRow is FromRow + 1,
    ToRow =< 7,
    get_piece(Board, ToRow, Col, e).

% black pawn: two steps from starting row (row 1)
legal_move(Board, black, 1, Col, 3, Col) :-
    get_piece(Board, 2, Col, e),
    get_piece(Board, 3, Col, e).

% black pawn: diagonal capture
legal_move(Board, black, FromRow, FromCol, ToRow, ToCol) :-
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