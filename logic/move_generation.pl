% generate_piece_moves(+Board, +Color, +FromR, +FromC, +Piece, +LastMove, +CastleRights, -Moves)

generate_piece_moves(Board, Color, FromR, FromC, Piece, LastMove, CastleRights, Moves) :-
    ( Piece = 'P' ; Piece = 'p' ) ->
        generate_pawn_moves(Board, Color, FromR, FromC, LastMove, Moves) ;
    ( Piece = 'N' ; Piece = 'n' ) ->
        generate_knight_moves(Board, Color, FromR, FromC, Moves) ;
    ( Piece = 'B' ; Piece = 'b' ) ->
        generate_bishop_moves(Board, Color, FromR, FromC, Moves) ;
    ( Piece = 'R' ; Piece = 'r' ) ->
        generate_rook_moves(Board, Color, FromR, FromC, Moves) ;
    ( Piece = 'Q' ; Piece = 'q' ) ->
        generate_queen_moves(Board, Color, FromR, FromC, Moves) ;
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
generate_king_moves(Board, Color, FromR, FromC, LastMove, CastleRights, Moves) :-
    findall(move(FromR, FromC, ToR, ToC, none),
        king_move(Board, Color, FromR, FromC, ToR, ToC, LastMove, CastleRights),
        Moves).
generate_bishop_moves(Board, Color, FromR, FromC, Moves) :-
    findall(move(FromR, FromC, ToR, ToC, none),
        bishop_move(Board, Color, FromR, FromC, ToR, ToC, _),
        Moves).

generate_rook_moves(Board, Color, FromR, FromC, Moves) :-
    findall(move(FromR, FromC, ToR, ToC, none),
        rook_move(Board, Color, FromR, FromC, ToR, ToC, _),
        Moves).

generate_queen_moves(Board, Color, FromR, FromC, Moves) :-
    findall(move(FromR, FromC, ToR, ToC, none),
        queen_move(Board, Color, FromR, FromC, ToR, ToC, _),
        Moves).
