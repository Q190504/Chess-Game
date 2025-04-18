% ------------------------------
% Helpers
% ------------------------------

% Checks if a square is empty
is_empty(e).

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

% ------------------------------
% Board manipulation logic
% Get, set piece
% ------------------------------
% Gets a piece at (Row, Col) from the Board
get_piece(Board, Row, Col, Piece) :-
    nth0(Row, Board, BoardRow),
    nth0(Col, BoardRow, Piece).

% Set new piece in its new pos
set_piece(Board, R, C, Piece, NewBoard) :-
    replace_in_matrix(Board, R, C, Piece, NewBoard).

% Replace a member of a matrix
replace_in_matrix(Matrix, RowIndex, ColIndex, Elem, NewMatrix) :-
    nth0(RowIndex, Matrix, OldRow),
    replace_in_list(OldRow, ColIndex, Elem, NewRow),
    replace_in_list(Matrix, RowIndex, NewRow, NewMatrix).

% Replace a member of a list
replace_in_list(List, Index, Elem, NewList) :-
    same_length(List, NewList),
    append(Prefix, [_|Suffix], List),
    length(Prefix, Index),
    append(Prefix, [Elem|Suffix], NewList).

% ------------------------------
% Legal Move Logic
% A move is considered legal if it is following the move rule of the piece
% ------------------------------
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

% ------------------------------
% Safe Move Logic
% A move is considered safe if it is a legal move and not put the king of it side on check
% ------------------------------

safe_move(Board, Color, R, C, ToR, ToC, LastMove) :-
    legal_move(Board, Color, R, C, ToR, ToC, LastMove),
    simulate_move(Board, Color, R, C, ToR, ToC, LastMove, NewBoard), % Perform the move on the current board 
    \+ in_check(NewBoard, Color). % Check if it put the king in check

% Simulate move: perform the move on the current board
simulate_move(Board, Color, R, C, ToR, ToC, LastMove, NewBoard) :-
    get_piece(Board, R, C, Piece),
    Piece \= e,

    % Check and remove for en passant move, castling move
    (
        en_passant(Board, Color, R, C, ToR, ToC, LastMove) ->
            (
                (Color = white -> CapR is ToR + 1 ; CapR is ToR - 1),
                set_piece(Board, CapR, ToC, e, TempBoard1), % Clear captured pawn
                set_piece(TempBoard1, R, C, e, TempBoard2) % Old Pos to empty
            )
        ;
        set_piece(Board, R, C, e, TempBoard2) % Old Pos to empty
    ),
    set_piece(TempBoard2, ToR, ToC, Piece, NewBoard). % New Pos to piece

% ------------------------------
% Check Logic
% Check if any opponent piece has a legal move to the king square.
% ------------------------------

% Check if king is in check
in_check(Board, Color) :-
    find_king(Board, Color, Row, Col),
    under_attack(Board, Color, Row, Col).

% find_king(Board, Color, Row, Col)
find_king(Board, Color, Row, Col) :-
    king_symbol(Color, Symbol),
    find_king_in_rows(Board, Symbol, 0, Row, Col).

% king_symbol(Color, Symbol)
king_symbol(white, 'K').
king_symbol(black, 'k').

% find_king_in_rows(Board, Symbol, CurrentRowNum, RowNum, ColNum)
find_king_in_rows([CurrentRow|_], Symbol, CurrentRowNum, CurrentRowNum, ColNum) :-
    nth0(ColNum, CurrentRow, Symbol).

find_king_in_rows([_|RestRows], Symbol, CurrentRowNum, RowNum, ColNum) :-
    NextRowNum is CurrentRowNum + 1,
    find_king_in_rows(RestRows, Symbol, NextRowNum, RowNum, ColNum).

% Check if opponent can move there
under_attack(Board, Color, TargetRow, TargetCol) :-
    opponent_color(Color, OppColor),
    between(0, 7, Row),
    between(0, 7, Col),
    nth0(Row, Board, BoardRow),
    nth0(Col, BoardRow, Piece),
    Piece \= e,
    piece_color(Piece, OppColor),
    % format('Trying move from (~w,~w) ~w to king at (~w,~w)~n', [Row, Col, Piece, TargetRow, TargetCol]),
    legal_move(Board, OppColor, Row, Col, TargetRow, TargetCol, _),
    % format('LEGAL move from ~w at (~w,~w) to (~w,~w)~n', [Piece, Row, Col, TargetRow, TargetCol]),
    !.


% Define opponent color
opponent_color(white, black).
opponent_color(black, white).

% Define piece color
piece_color(Piece, white) :- atom_chars(Piece, [C]), char_type(C, upper).
piece_color(Piece, black) :- atom_chars(Piece, [C]), char_type(C, lower).


% ------------------------------
% Checkmate Logic
% Check if the king is in check and has no safe moves to escape.
% ------------------------------

checkmate(Board, Color, LastMove) :-
    in_check(Board, Color),
    \+ has_safe_move(Board, Color, LastMove).

% True if there is at least one legal move that is also safe (i.e., doesnt leave king in check)
has_safe_move(Board, Color, LastMove) :-
    member(R, [0,1,2,3,4,5,6,7]),
    member(C, [0,1,2,3,4,5,6,7]),
    get_piece(Board, R, C, Piece),
    piece_color(Piece, Color),
    member(ToR, [0,1,2,3,4,5,6,7]),
    member(ToC, [0,1,2,3,4,5,6,7]),
    safe_move(Board, Color, R, C, ToR, ToC, LastMove),
    !.  % Cut: we only need one


% ------------------------------
% Stalemate Logic
% Check if the king is not in check and has no legal moves to escape.
% ------------------------------
stalemate(Board, Color, LastMove) :-
    \+ in_check(Board, Color),
    \+ has_safe_move(Board, Color, LastMove).