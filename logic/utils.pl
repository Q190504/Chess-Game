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


% Legal Move Checker
% Legal Move Checker
% white
legal_move(Board, white, R, C, ToR, ToC, LastMove) :- 
    get_piece(Board, R, C, Piece),
    (
        Piece = 'P', pawn_move(Board, white, R, C, ToR, ToC, LastMove)
    ;   Piece = 'N', knight_move(Board, white, R, C, ToR, ToC, LastMove)
    ;   Piece = 'R', rook_move(Board, white, R, C, ToR, ToC, LastMove)
    ;   Piece = 'B', bishop_move(Board, white, R, C, ToR, ToC, LastMove)
    ;   Piece = 'Q', queen_move(Board, white, R, C, ToR, ToC, LastMove)
    ;   Piece = 'K', king_move(Board, white, R, C, ToR, ToC, LastMove)
    ).

% black
legal_move(Board, black, R, C, ToR, ToC, LastMove) :- 
    get_piece(Board, R, C, Piece),
    (
        Piece = 'p', pawn_move(Board, black, R, C, ToR, ToC, LastMove)
    ;   Piece = 'n', knight_move(Board, black, R, C, ToR, ToC, LastMove)
    ;   Piece = 'r', rook_move(Board, black, R, C, ToR, ToC, LastMove)
    ;   Piece = 'b', bishop_move(Board, black, R, C, ToR, ToC, LastMove)
    ;   Piece = 'q', queen_move(Board, black, R, C, ToR, ToC, LastMove)
    ;   Piece = 'k', king_move(Board, black, R, C, ToR, ToC, LastMove)
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
