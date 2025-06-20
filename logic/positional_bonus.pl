reverse_table(table(R1,R2,R3,R4,R5,R6,R7,R8), table(R8,R7,R6,R5,R4,R3,R2,R1)).

pawn_table_black(
    table(
        row(  0,   0,   0,   0,   0,   0,   0,   0),
        row(  5,  10,  10, -20, -20,  10,  10,   5),
        row(  5,  -5, -10,   0,   0, -10,  -5,   5),
        row(  0,   0,   0,  20,  20,   0,   0,   0),
        row(  5,   5,  10,  25,  25,  10,   5,   5),
        row( 10,  10,  20,  30,  30,  20,  10,  10),
        row( 50,  50,  50,  50,  50,  50,  50,  50),
        row(  0,   0,   0,   0,   0,   0,   0,   0)
    )
).

pawn_table_white(Table) :-
    pawn_table_black(WT),
    reverse_table(WT, Table).

knight_table_black( table(
    row(-50, -40, -30, -30, -30, -30, -40, -50),
    row(-40, -20,   0,   0,   0,   0, -20, -40),
    row(-30,   0,  10,  15,  15,  10,   0, -30),
    row(-30,   5,  15,  20,  20,  15,   5, -30),
    row(-30,   0,  15,  20,  20,  15,   0, -30),
    row(-30,   5,  10,  15,  15,  10,   5, -30),
    row(-40, -20,   0,   5,   5,   0, -20, -40),
    row(-50, -40, -30, -30, -30, -30, -40, -50)
)).
knight_table_white(Table) :-
    knight_table_black(WT),
    reverse_table(WT, Table).

bishop_table_black( table(
    row(-20, -10, -10, -10, -10, -10, -10, -20),
    row(-10,   5,   0,   0,   0,   0,   5, -10),
    row(-10,  10,  10,  10,  10,  10,  10, -10),
    row(-10,   0,  10,  10,  10,  10,   0, -10),
    row(-10,   5,   5,  10,  10,   5,   5, -10),
    row(-10,   0,   5,  10,  10,   5,   0, -10),
    row(-10,   0,   0,   0,   0,   0,   0, -10),
    row(-20, -10, -10, -10, -10, -10, -10, -20)
)).
bishop_table_white(Table) :-
    bishop_table_black(WT),
    reverse_table(WT, Table).

rook_table_black( table(
    row(  0,   0,   0,   0,   0,   0,   0,   0),
    row(  5,  10,  10,  10,  10,  10,  10,   5),
    row( -5,   0,   0,   0,   0,   0,   0,  -5),
    row( -5,   0,   0,   0,   0,   0,   0,  -5),
    row( -5,   0,   0,   0,   0,   0,   0,  -5),
    row( -5,   0,   0,   0,   0,   0,   0,  -5),
    row( -5,   0,   0,   0,   0,   0,   0,  -5),
    row(  0,   0,   0,   5,   5,   0,   0,   0)
)).
rook_table_white(Table) :-
    rook_table_black(WT),
    reverse_table(WT, Table).

queen_table_black( table(
    row(-20, -10, -10,  -5,  -5, -10, -10, -20),
    row(-10,   0,   5,   0,   0,   0,   0, -10),
    row(-10,   5,   5,   5,   5,   5,   0, -10),
    row(  0,   0,   5,   5,   5,   5,   0,  -5),
    row( -5,   0,   5,   5,   5,   5,   0,  -5),
    row(-10,   0,   5,   5,   5,   5,   0, -10),
    row(-10,   0,   0,   0,   0,   0,   0, -10),
    row(-20, -10, -10,  -5,  -5, -10, -10, -20)
)).
queen_table_white(Table) :-
    queen_table_black(WT),
    reverse_table(WT, Table).

king_table_black( table(
    row(-30, -40, -40, -50, -50, -40, -40, -30),
    row(-30, -40, -40, -50, -50, -40, -40, -30),
    row(-30, -40, -40, -50, -50, -40, -40, -30),
    row(-30, -40, -40, -50, -50, -40, -40, -30),
    row(-20, -30, -30, -40, -40, -30, -30, -20),
    row(-10, -20, -20, -20, -20, -20, -20, -10),
    row( 20,  20,   0,   0,   0,   0,  20,  20),
    row( 20,  30,  10,   0,   0,  10,  30,  20)
)).
king_table_white(Table) :-
    king_table_black(WT),
    reverse_table(WT, Table).


% Rook tables
piece_positional_table('R', Table) :- rook_table_white(Table).
piece_positional_table('r', Table) :- rook_table_black(Table).

% Bishop tables
piece_positional_table('B', Table) :- bishop_table_white(Table).
piece_positional_table('b', Table) :- bishop_table_black(Table).

% Knight tables
piece_positional_table('N', Table) :- knight_table_white(Table).
piece_positional_table('n', Table) :- knight_table_black(Table).

% Pawn tables
piece_positional_table('P', Table) :- pawn_table_white(Table).
piece_positional_table('p', Table) :- pawn_table_black(Table).

% Queen tables
piece_positional_table('Q', Table) :- queen_table_white(Table).
piece_positional_table('q', Table) :- queen_table_black(Table).

% King tables
piece_positional_table('K', Table) :- king_table_white(Table).
piece_positional_table('k', Table) :- king_table_black(Table).

positional_bonus(Color, Board, Score) :-
    evaluate_positional(Board, 0, 0, Color, 0, Score).

evaluate_positional([], _, _, _, Acc, Acc).
evaluate_positional([Row|Rest], R, _, Color, Acc, Score) :-
    evaluate_row_positional(Row, R, 0, Color, 0, RowScore),
    NewAcc is Acc + RowScore,
    R1 is R + 1,
    evaluate_positional(Rest, R1, 0, Color, NewAcc, Score).

evaluate_row_positional([], _, _, _, Acc, Acc).
evaluate_row_positional([Piece|Rest], R, C, Color, Acc, Score) :-
    ( belongs_to(Piece, Color) ->
        positional_value(Piece, R, C, V)
    ; V = 0 ),
    Acc1 is Acc + V,
    C1 is C + 1,
    evaluate_row_positional(Rest, R, C1, Color, Acc1, Score).

% helper: check piece belongs to color
belongs_to(Piece, white) :- member(Piece, ['P','N','B','R','Q','K']).
belongs_to(Piece, black) :- member(Piece, ['p','n','b','r','q','k']).


fast_table_lookup(table(Row1,Row2,Row3,Row4,Row5,Row6,Row7,Row8), R, C, V) :-
    R1 is R + 1,  % vì arg bắt đầu từ 1
    C1 is C + 1,
    arg(R1, table(Row1,Row2,Row3,Row4,Row5,Row6,Row7,Row8), Row),
    arg(C1, Row, V).

positional_value(Piece, R, C, V) :-
    piece_positional_table(Piece, Table),
    fast_table_lookup(Table, R, C, V).
positional_value(e, _, _, 0).  % empty square

