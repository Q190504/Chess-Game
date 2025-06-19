
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

move_score(Board, Color, move(FR, FC, TR, TC, Promo), Score) :-
    (
        (Color == white,
         (Promo == 'Q' -> PromoBonus = 1000 ;
          Promo == 'N' -> PromoBonus = 900 ;
          Promo == 'R' -> PromoBonus = 800 ;
          Promo == 'B' -> PromoBonus = 700 ;
          PromoBonus = 0))
    ;
        (Color == black,
         (Promo == 'q' -> PromoBonus = 1000 ;
          Promo == 'n' -> PromoBonus = 900 ;
          Promo == 'r' -> PromoBonus = 800 ;
          Promo == 'b' -> PromoBonus = 700 ;
          PromoBonus = 0))
    ),
    Score is PromoBonus,
    log("Move ~w - ~w with promo ~w scored: ~w~n", [[FR, FC], [TR, TC], Promo, Score]).


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
    findall(
        move(FromR, FromC, ToR, ToC, Promo),
        (
            member((FromR, FromC, Piece), MyPieces),
            between(0, 7, ToR), between(0, 7, ToC),
            legal_move(Board, Color, FromR, FromC, ToR, ToC, LastMove, CastleRights),
            generate_promotion(Piece, Color, ToR, Promo),
            simulate_and_validate(Board, Color, FromR, FromC, ToR, ToC, LastMove, Promo, CastleRights, _NewCastle, SimBoard),
            \+ in_check(SimBoard, Color)
        ),
        Moves
    ).

generate_promotion('P', white, 0, Promo) :- member(Promo, ['N','B','R','Q']).
generate_promotion('p', black, 7, Promo) :- member(Promo, ['n','b','r','q']).
generate_promotion(_, _, _, none).

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

