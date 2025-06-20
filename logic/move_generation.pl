% generate_piece_moves(+Board, +Color, +FromR, +FromC, +Piece, +LastMove, +CastleRights, -Moves)

generate_piece_moves(Board, Color, FromR, FromC, Piece, LastMove, CastleRights, Moves) :-
    ( Piece = 'P' ; Piece = 'p' ) ->
        generate_pawn_moves(Board, Color, FromR, FromC, LastMove, Moves) ;
    ( Piece = 'N' ; Piece = 'n' ) ->
        generate_knight_moves(Board, Color, FromR, FromC, Moves) ;
    ( Piece = 'B' ; Piece = 'b' ) ->
        generate_sliding_moves(Board, Color, FromR, FromC, [ (1,1), (1,-1), (-1,1), (-1,-1) ], Moves) ;
    ( Piece = 'R' ; Piece = 'r' ) ->
        generate_sliding_moves(Board, Color, FromR, FromC, [ (1,0), (-1,0), (0,1), (0,-1) ], Moves) ;
    ( Piece = 'Q' ; Piece = 'q' ) ->
        generate_sliding_moves(Board, Color, FromR, FromC, [ (1,0), (-1,0), (0,1), (0,-1), (1,1), (1,-1), (-1,1), (-1,-1) ], Moves) ;
    ( Piece = 'K' ; Piece = 'k' ) ->
        generate_king_moves(Board, Color, FromR, FromC, LastMove, CastleRights, Moves).

generate_pawn_moves(Board, Color, FromR, FromC, LastMove, Moves) :-
    findall(move(FromR, FromC, ToR, ToC, none),
        pawn_move(Board, Color, FromR, FromC, ToR, ToC, LastMove),
        Moves).
generate_knight_moves(Board, Color, FromR, FromC, Moves) :-
    findall(move(FromR, FromC, ToR, ToC, none),
        knight_move(Board, Color, FromR, FromC, ToR, ToC, _),
        Moves).
generate_sliding_moves(Board, Color, FromR, FromC, Directions, Moves) :-
    findall(move(FromR, FromC, ToR, ToC, none),
        (
            member((Dr, Dc), Directions),
            slide_target(Board, Color, FromR, FromC, Dr, Dc, ToR, ToC)
        ),
        Moves).

slide_target(Board, Color, R, C, Dr, Dc, ToR, ToC) :-
    R1 is R + Dr,
    C1 is C + Dc,
    exist_sqr(R1, C1),
    get_piece(Board, R1, C1, P),
    (
        P = e -> (ToR = R1, ToC = C1) ;
        opponent(Color, P) -> (ToR = R1, ToC = C1) ;
        fail
    ).
generate_king_moves(Board, Color, FromR, FromC, LastMove, CastleRights, Moves) :-
    findall(move(FromR, FromC, ToR, ToC, none),
        king_move(Board, Color, FromR, FromC, ToR, ToC, LastMove, CastleRights),
        Moves).
