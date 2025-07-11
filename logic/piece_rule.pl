% ------------------------------
% Pawn
% A white pawn can move one step forward to an empty square or two steps forward from its starting position (row 6).
% It can also capture an opponent piece diagonally to the left or right.
% ------------------------------

% white pawn: one step
pawn_move(Board, white, FromRow, Col, ToRow, Col, _LastMove) :-
    ToRow is FromRow - 1,
    ToRow >= 0,
    get_piece(Board, ToRow, Col, e).

% white pawn: two steps from starting row (row 6)
pawn_move(Board, white, 6, Col, 4, Col, _LastMove) :-
    get_piece(Board, 5, Col, e),
    get_piece(Board, 4, Col, e).

% white pawn: diagonal capture
pawn_move(Board, white, FromRow, FromCol, ToRow, ToCol, _LastMove) :-
    ToRow is FromRow - 1,
    ToRow >= 0,
    (ToCol is FromCol - 1 ; ToCol is FromCol + 1),
    ToCol >= 0, ToCol =< 7,
    get_piece(Board, ToRow, ToCol, Piece),
    Piece \= e,
    is_black(Piece).

% white pawn: en passant capture
pawn_move(Board, white, FromRow, FromCol, ToRow, ToCol, LastMove) :-
    FromRow = 3,  % white pawn must be on 3rd rank
    ToRow = 2,
    (ToCol is FromCol - 1 ; ToCol is FromCol + 1),
    get_piece(Board, ToRow, ToCol, e),  % destination must be empty
    LastMove = move(1, ToCol, 3, ToCol, 'p').  % black pawn just moved first move

% black pawn: one step
pawn_move(Board, black, FromRow, Col, ToRow, Col, _LastMove) :-
    ToRow is FromRow + 1,
    ToRow =< 7,
    get_piece(Board, ToRow, Col, e).

% black pawn: two steps from starting row (row 1)
pawn_move(Board, black, 1, Col, 3, Col, _LastMove) :-
    get_piece(Board, 2, Col, e),
    get_piece(Board, 3, Col, e).

% black pawn: diagonal capture
pawn_move(Board, black, FromRow, FromCol, ToRow, ToCol, _LastMove) :-
    ToRow is FromRow + 1,
    ToRow =< 7,
    (ToCol is FromCol - 1 ; ToCol is FromCol + 1),
    ToCol >= 0, ToCol =< 7,
    get_piece(Board, ToRow, ToCol, Piece),
    Piece \= e,
    is_white(Piece).

% black pawn: en passant capture
pawn_move(Board, black, FromRow, FromCol, ToRow, ToCol, LastMove) :-
    FromRow = 4,  % black pawn must be on 4th rank
    ToRow = 5,
    (ToCol is FromCol - 1 ; ToCol is FromCol + 1),
    get_piece(Board, ToRow, ToCol, e),  % destination must be empty
    LastMove = move(6, ToCol, 4, ToCol, 'P'). % white pawn just moved first move

% ------------------------------
% En Passant Logic
% The en passant move is already defined in pawn_move, this is for checking if the move is en passant
% Returns true if a pawn at (FromRow, FromCol) can perform en passant to (ToRow, ToCol)
% ------------------------------

% White
en_passant(Board, white, FromRow, FromCol, ToRow, ToCol, LastMove) :-
    get_piece(Board, FromRow, FromCol, 'P'),  % source must be a white pawn
    FromRow = 3,  % white pawn must be on 3rd rank
    ToRow = 2,
    (ToCol is FromCol - 1 ; ToCol is FromCol + 1),
    get_piece(Board, ToRow, ToCol, e),  % destination must be empty
    LastMove = move(1, ToCol, 3, ToCol, 'p').  % black pawn just moved first move

% Black
en_passant(Board, black, FromRow, FromCol, ToRow, ToCol, LastMove) :-
    get_piece(Board, FromRow, FromCol, 'p'),  % source must be a white pawn
    FromRow = 4,  % black pawn must be on 4th rank
    ToRow = 5,
    (ToCol is FromCol - 1 ; ToCol is FromCol + 1),
    get_piece(Board, ToRow, ToCol, e),  % destination must be empty
    LastMove = move(6, ToCol, 4, ToCol, 'P'). % white pawn just moved first move

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

knight_move(Board, Color, FromRow, FromCol, ToRow, ToCol, _LastMove) :-
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

rook_move(Board, Color, FromRow, FromCol, ToRow, ToCol, _LastMove) :-
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

bishop_move(Board, Color, FromRow, FromCol, ToRow, ToCol, _LastMove) :-
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

queen_move(Board, Color, FromRow, FromCol, ToRow, ToCol, _LastMove) :-
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
king_move(Board, Color, FromRow, FromCol, ToRow, ToCol, _LastMove, _CastleRights) :-
    king_offset(Dr, Dc),
    ToRow is FromRow + Dr,
    ToCol is FromCol + Dc,
    exist_sqr(ToRow, ToCol),
    get_piece(Board, ToRow, ToCol, Target),
    (Target = e ; opponent(Color, Target)).

% Castling move
king_move(Board, Color, FromRow, FromCol, ToRow, ToCol, _LastMove, CastleRights) :-
    king_castle_move(Board, Color, FromRow, FromCol, ToRow, ToCol, CastleRights).

% Check for castling: Neither king/ rook moved before, no square is in check between them, no square is occupied between them
% king_castle_move(+Board, +Color, +FromRow, +FromCol, -ToRow, -ToCol, +CastleRights)

king_castle_move(Board, white, 7, 4, 7, 6, CastleRights) :-  % White kingside
    CastleRights = castle_rights(true,_,_,_),
    get_piece(Board, 7, 5, e),
    get_piece(Board, 7, 6, e),
    \+ under_attack(Board, white, 7, 4),  % King is not in check
    \+ under_attack(Board, white, 7, 5),
    \+ under_attack(Board, white, 7, 6).

king_castle_move(Board, white, 7, 4, 7, 2, CastleRights) :-  % White queenside
    CastleRights = castle_rights(_,true,_,_),
    get_piece(Board, 7, 1, e),
    get_piece(Board, 7, 2, e),
    get_piece(Board, 7, 3, e),
    \+ under_attack(Board, white, 7, 4),
    \+ under_attack(Board, white, 7, 3),
    \+ under_attack(Board, white, 7, 2).

king_castle_move(Board, black, 0, 4, 0, 6, CastleRights) :-  % Black kingside
    CastleRights = castle_rights(_,_,true,_),
    get_piece(Board, 0, 5, e),
    get_piece(Board, 0, 6, e),
    \+ under_attack(Board, black, 0, 4),
    \+ under_attack(Board, black, 0, 5),
    \+ under_attack(Board, black, 0, 6).

king_castle_move(Board, black, 0, 4, 0, 2, CastleRights) :-  % Black queenside
    CastleRights = castle_rights(_,_,_,true),
    get_piece(Board, 0, 1, e),
    get_piece(Board, 0, 2, e),
    get_piece(Board, 0, 3, e),
    \+ under_attack(Board, black, 0, 4),
    \+ under_attack(Board, black, 0, 3),
    \+ under_attack(Board, black, 0, 2).


% castling_move(+Board, +Color, +R, +C, +ToR, +ToC, +Piece, -NewBoard)
castling_move(Board, white, 7, 4, _, 6, 'K', NewBoard) :-  % Kingside white
    multi_set_piece(Board,
        [(7,4,e), (7,7,e), (7,6,'K'), (7,5,'R')],
        NewBoard).
castling_move(Board, white, 7, 4, _, 2, 'K', NewBoard) :-  % Queenside white
    multi_set_piece(Board,
        [(7,4,e), (7,0,e), (7,2,'K'), (7,3,'R')],
        NewBoard).
castling_move(Board, black, 0, 4, _, 6, 'k', NewBoard) :-  % Kingside black
    multi_set_piece(Board,
        [(0,4,e), (0,7,e), (0,6,'k'), (0,5,'r')],
        NewBoard).
castling_move(Board, black, 0, 4, _, 2, 'k', NewBoard) :-  % Queenside black
    multi_set_piece(Board,
        [(0,4,e), (0,0,e), (0,2,'k'), (0,3,'r')],
        NewBoard).
