
%------------------------
% Piece ownership
%------------------------
piece_belongs_to_color('P', white).
piece_belongs_to_color('N', white).
piece_belongs_to_color('B', white).
piece_belongs_to_color('R', white).
piece_belongs_to_color('Q', white).
piece_belongs_to_color('K', white).
piece_belongs_to_color('p', black).
piece_belongs_to_color('n', black).
piece_belongs_to_color('b', black).
piece_belongs_to_color('r', black).
piece_belongs_to_color('q', black).
piece_belongs_to_color('k', black).


%------------------------
% Logging switch
%------------------------
:- dynamic debug_mode/1.
debug_mode(off).

log(Msg, Args) :-
    debug_mode(on), !,
    format(Msg, Args).
log(_, _).

%------------------------
% Evaluation
%------------------------
% Entry point
evaluate(Board, Color, Score) :-
    piece_bonus(Color, Board, MaterialScore),
    positional_bonus(Color, Board, PositionalScore),
    opponent_color(Color, OpponentColor),
    (stalemate(Board, OpponentColor, _, _) -> StalematePenalty = -50 ; StalematePenalty = 0),
    Score is MaterialScore + PositionalScore + StalematePenalty.

%------------------------
% Entry point
%------------------------
minimax(Board, Color, Depth, LastMove, CastleRights, BestMove, Value) :-
    negamax_search(Board, Color, Depth, LastMove, CastleRights, -10000, 10000, BestMove, Value).


%------------------------
% Minimax with pruning
%------------------------
score_and_sort_moves(Board, Color, Moves, SortedMoves) :-
    map_list_to_pairs(move_score(Board, Color), Moves, ScoredMoves),
    keysort(ScoredMoves, SortedPairs),
    reverse(SortedPairs, Descending),  % higher scores first
    pairs_values(Descending, SortedMoves).

promo_score('Q', 900). promo_score('R', 500).
promo_score('B', 300). promo_score('N', 300).
promo_score('q', 900). promo_score('r', 500).
promo_score('b', 300). promo_score('n', 300).

move_score(_Board, _Color, move(_, _, _TR, _TC, Promo), Score) :-
    (Promo \= none -> promo_score(Promo, PromoScore) ; PromoScore = 0),
    Score is PromoScore.

find_pieces(Board, Color, Pieces) :-
    findall((R, C, Piece),
        (
            nth0(R, Board, Row),
            nth0(C, Row, Piece),
            piece_belongs_to_color(Piece, Color)
        ),
    Pieces).

generate_all_moves(Board, Color, LastMove, CastleRights, Moves) :-
    find_pieces(Board, Color, MyPieces),
    findall(Move,
        (
            member((FromR, FromC, Piece), MyPieces),
            generate_piece_moves(Board, Color, FromR, FromC, Piece, LastMove, CastleRights, RawMoves),
            member(Move0, RawMoves),
            generate_promotion(Piece, Color, Move0, MoveWithPromo),
            MoveWithPromo = move(FR, FC, TR, TC, Promo),
            simulate_and_validate(Board, Color, FR, FC, TR, TC, LastMove, Promo, CastleRights, _NewCastle, SimBoard),
            \+ in_check(SimBoard, Color),
            Move = MoveWithPromo
        ),
        Moves).


generate_promotion('P', white, move(FR, FC, TR, TC, none), move(FR, FC, TR, TC, Promo)) :-
    TR =:= 0, member(Promo, ['Q', 'R', 'B', 'N']).

generate_promotion('p', black, move(FR, FC, TR, TC, none), move(FR, FC, TR, TC, Promo)) :-
    TR =:= 7, member(Promo, ['q', 'r', 'b', 'n']).

generate_promotion(_, _, Move, Move).  % không phong


simulate_and_validate(Board, Color, FromR, FromC, ToR, ToC, LastMove, Promo, CastleRights, NewCastle, SimBoard) :-
    simulate_move(Board, Color, FromR, FromC, ToR, ToC, LastMove, Promo, CastleRights, NewCastle, SimBoard).

negamax_search(Board, Color, Depth, LastMove, CastleRights, Alpha, Beta, BestMove, BestValue) :-
    (Depth =< 0 ->
        evaluate(Board, Color, Value),
        BestMove = none,
        BestValue = Value,
        log("Depth limit. Eval: ~w~n", [Value])
    ;
        generate_all_moves(Board, Color, LastMove, CastleRights, Moves),
        score_and_sort_moves(Board, Color, Moves, SortedMoves),
        (Moves = [] ->
            (in_check(Board, Color) ->
                BestValue = -9999, BestMove = none
            ;
                BestValue = 0, BestMove = none
            )
        ;
            negamax_loop(SortedMoves, Board, Color, Depth, Alpha, Beta, LastMove, CastleRights, BestMove, BestValue)
        )
    ).

negamax_loop([], _Board, _Color, _Depth, _Alpha, _Beta, _LastMove, _CastleRights, none, -10000).

negamax_loop([Move|Rest], Board, Color, Depth, Alpha, Beta, LastMove, CastleRights, BestMove, BestValue) :-
    Move = move(FromR, FromC, ToR, ToC, Promo),
    (
        simulate_move(Board, Color, FromR, FromC, ToR, ToC, LastMove, Promo, CastleRights, NewCastle, SimBoard)
    ),
    Depth1 is Depth - 1,
    % Negamax recursive call
    opponent_color(Color, OppColor),
    negamax_search(SimBoard, OppColor, Depth1, Move, NewCastle, -Beta, -Alpha, _, OppValue),
    Value is -OppValue,
    (
        Value > Alpha ->
            NewAlpha = Value,
            NewBestMove = Move
        ;
            NewAlpha = Alpha,
            NewBestMove = BestMove
    ),
    (NewAlpha >= Beta ->
        BestMove = NewBestMove, BestValue = NewAlpha,
        log("Beta cutoff at move: ~w~n", [NewBestMove])
    ;
        negamax_loop(Rest, Board, Color, Depth, NewAlpha, Beta, LastMove, CastleRights, TempMove, TempValue),
        (TempValue > NewAlpha ->
            BestMove = TempMove, BestValue = TempValue
        ;
            BestMove = NewBestMove, BestValue = NewAlpha
        )
    ).

