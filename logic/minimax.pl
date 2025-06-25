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
% Logging
%------------------------
:- dynamic debug_mode/1.
debug_mode(on).

:- dynamic debug_file_mode/1.
debug_file_mode(off).

log(Msg, Args) :-
    debug_mode(on), !,
    format(Msg, Args).
log(_, _).

log_to_file(Msg, Args) :-
    debug_file_mode(on), !,
    setup_call_cleanup(
        open('log.txt', append, Stream),
        format(Stream, Msg, Args),
        close(Stream)
    ).
log_to_file(_, _).

%------------------------
% Evaluation
%------------------------
evaluate(Board, Color, Score) :-
    piece_bonus(Color, Board, MaterialScore),
    positional_bonus(Color, Board, PositionalScore),
    % king_safety_bonus(Color, Board, KingSafetyScore),
    Score is MaterialScore + PositionalScore.

%------------------------
% Entry point for Minimax
%------------------------
minimax(Board, Color, Depth, LastMove, CastleRights, History, BestMove, Value) :-
    retractall(tt_entry(_, _, _, _)),
    negamax_search(Board, Color, Depth, LastMove, CastleRights, History, -10000, 10000, BestMove, Value).

%------------------------
% Minimax with Alpha-Beta Pruning
%------------------------
score_and_sort_moves(Board, Color, Moves, SortedMoves) :-
    map_list_to_pairs(move_score(Board, Color), Moves, ScoredMoves),
    keysort(ScoredMoves, SortedPairs),
    reverse(SortedPairs, Descending),
    pairs_values(Descending, SortedMoves).

promo_score('Q', 900). promo_score('R', 500).
promo_score('B', 300). promo_score('N', 300).
promo_score('q', 900). promo_score('r', 500).
promo_score('b', 300). promo_score('n', 300).

move_score(_, _, move(_, _, _, _, Promo, _), Score) :-
    (Promo \= none -> promo_score(Promo, PromoScore) ; PromoScore = 0),
    Score is PromoScore.

find_pieces(Board, Color, Pieces) :-
    findall((R, C, Piece),
        (nth0(R, Board, Row), nth0(C, Row, Piece), piece_belongs_to_color(Piece, Color)),
        Pieces).

generate_all_moves(Board, Color, LastMove, CastleRights, Moves) :-
    find_pieces(Board, Color, MyPieces),
    length(MyPieces, PieceCount),
    log_to_file("[MOVEGEN] Generating moves for ~w (~w pieces)~n", [Color, PieceCount]),

    findall(Move,
        (
            member((FromR, FromC, Piece), MyPieces),
            log_to_file("[MOVEGEN] Generating moves for piece ~w at (~w,~w)~n", [Piece, FromR, FromC]),

            generate_piece_moves(Board, Color, FromR, FromC, Piece, LastMove, CastleRights, RawMoves),
            length(RawMoves, RawCount),
            log_to_file("[MOVEGEN] → ~w raw moves generated~n", [RawCount]),

            member(Move0, RawMoves),
            generate_promotion(Piece, Color, Move0, MoveWithPromo0),
            MoveWithPromo0 = move(FR, FC, TR, TC, Promo),

            (simulate_and_validate(Board, Color, FR, FC, TR, TC, LastMove, Promo, CastleRights, _, SimBoard),
             \+ in_check(SimBoard, Color) ->
                true
            ;
                log_to_file("[MOVEGEN] Move from (~w,~w) to (~w,~w) would result in check, discarded~n", [FR, FC, TR, TC]),
                fail
            ),

            log_to_file("[MOVEGEN] Legal move: ~w~n", [move(FR, FC, TR, TC, Promo, Piece)]),
            Move = move(FR, FC, TR, TC, Promo, Piece)
        ),
        Moves),

    length(Moves, MoveCount),
    log_to_file("[MOVEGEN] Total legal moves for ~w: ~w~n", [Color, MoveCount]).

generate_promotion('P', white, move(FR, FC, TR, TC, none), move(FR, FC, TR, TC, Promo)) :-
    TR =:= 0, member(Promo, ['Q', 'R', 'B', 'N']).
generate_promotion('p', black, move(FR, FC, TR, TC, none), move(FR, FC, TR, TC, Promo)) :-
    TR =:= 7, member(Promo, ['q', 'r', 'b', 'n']).
generate_promotion(_, _, Move, Move).

simulate_and_validate(Board, Color, FromR, FromC, ToR, ToC, LastMove, Promo, CastleRights, NewCastle, SimBoard) :-
    simulate_move(Board, Color, FromR, FromC, ToR, ToC, LastMove, Promo, CastleRights, NewCastle, SimBoard).

%------------------------
% Transposition Table
%------------------------
:- dynamic tt_entry/4.

rep_board_hash(Board, Color, LastMove, CastleRights, Hash) :-
    maybe_hash_lastmove_enpassant(Color, LastMove, EnPassantInfo),
    term_hash((Board, Color, CastleRights, EnPassantInfo), Hash),
    log_to_file("[HASH] Color: ~w | EPInfo: ~w | CastleRights: ~w | Hash: ~w~n", 
                [Color, EnPassantInfo, CastleRights, Hash]).

maybe_hash_lastmove_enpassant(Color, move(Piece, FromR, _, ToR, _), move(Piece, FromR, ToR)) :-
    is_opponent_pawn(Color, Piece),
    abs(FromR - ToR) =:= 2, !.
maybe_hash_lastmove_enpassant(_, _, none).

is_opponent_pawn(white, 'p').
is_opponent_pawn(black, 'P').

count_repetition(_, [], 0).
count_repetition(Hash, [H|T], Count) :-
    log_to_file("[REPETITION CHECK] Comparing Hash: ~w with Entry: ~w~n", [Hash, H]),
    count_repetition(Hash, T, RestCount),
    (H == Hash -> Count is RestCount + 1, log_to_file("Match found → Count: ~w~n", [Count]) ; Count = RestCount).

%------------------------
% Negamax Core
%------------------------
negamax_search(Board, Color, Depth, LastMove, CastleRights, History, Alpha, Beta, BestMove, BestValue) :-
    rep_board_hash(Board, Color, LastMove, CastleRights, Hash),
    log_to_file("[SEARCH] Color: ~w | Depth: ~w | Alpha: ~w | Beta: ~w~n", [Color, Depth, Alpha, Beta]),

    ( Depth > 0, tt_entry(Hash, StoredDepth, StoredMove, StoredValue),
      nonvar(StoredDepth), StoredDepth >= Depth ->
        log_to_file("[TT-CUT] Hash: ~w | StoredDepth: ~w >= ~w | Skipping deeper search~n", [Hash, StoredDepth, Depth]),
        BestMove = StoredMove,
        BestValue = StoredValue
    ;
        ( Depth =< 0 ->
            evaluate(Board, Color, Value),
            BestMove = none,
            BestValue = Value
        ;
            generate_all_moves(Board, Color, LastMove, CastleRights, Moves),
            score_and_sort_moves(Board, Color, Moves, SortedMoves),
            ( SortedMoves = [] ->
                ( in_check(Board, Color) -> BestValue = -9999 ; BestValue = 0 ),
                BestMove = none
            ;
                negamax_loop(SortedMoves, Board, Color, Depth, Alpha, Beta, LastMove, CastleRights, History, Hash,
                             none, -10000, BestMove, BestValue)
            )
        ),

        ( Depth > 0 ->
            ( tt_entry(Hash, PrevDepth, _, _) ->
                ( nonvar(PrevDepth), PrevDepth < Depth -> retractall(tt_entry(Hash, _, _, _)), StoreTT = true ; StoreTT = false )
            ; StoreTT = true ),
            ( StoreTT ->
                assertz(tt_entry(Hash, Depth, BestMove, BestValue)),
                log_to_file("[TT-STORE] Hash: ~w | Depth: ~w | Move: ~w | Value: ~w~n", [Hash, Depth, BestMove, BestValue])
            ; true )
        ; true )
    ).

negamax_loop([], _, _, _, _, _, _, _, _, _, BestMove, BestValue, BestMove, BestValue).
negamax_loop([Move | Rest], Board, Color, Depth, Alpha, Beta, LastMove, CastleRights, History, CurrentHash,
             CurrentBestMove, CurrentBestValue, BestMove, BestValue) :-

    Move = move(FromR, FromC, ToR, ToC, Promo, Piece),
    simulate_move(Board, Color, FromR, FromC, ToR, ToC, LastMove, Promo, CastleRights, NewCastle, SimBoard),

    opponent_color(Color, OppColor),
    NextLastMove = move(FromR, FromC, ToR, ToC, Piece),

    rep_board_hash(SimBoard, OppColor, NextLastMove, NewCastle, NewHash),
    count_repetition(NewHash, History, Count),

    ( Count >= 2 -> Value = 0
    ;
        Depth1 is Depth - 1,
        negamax_search(SimBoard, OppColor, Depth1, NextLastMove, NewCastle, [NewHash | History], -Beta, -Alpha, _, OppValue),
        Value is -OppValue
    ),

    log_to_file("[EVAL] Move: ~w → Value: ~w, CurrentBest: ~w~n", [Move, Value, CurrentBestValue]),

    ( Value =:= 9999 ->
        log_to_file("[CHECKMATE] Found winning move ~w for turn ~w. returning immediately~n", [Move, Color]),
        BestMove = Move, BestValue = Value
    ;
        ( Value > CurrentBestValue ->
            NewAlpha = max(Alpha, Value),
            NewBestMove = Move,
            NewBestValue = Value
        ;
            NewAlpha = Alpha,
            NewBestMove = CurrentBestMove,
            NewBestValue = CurrentBestValue
        ),

        ( NewAlpha >= Beta ->
            log_to_file("[PRUNE] Alpha (~w) >= Beta (~w) → pruning remaining moves~n", [NewAlpha, Beta]),
            BestMove = NewBestMove, BestValue = NewBestValue
        ;
            negamax_loop(Rest, Board, Color, Depth, NewAlpha, Beta, LastMove, CastleRights, History, CurrentHash,
                         NewBestMove, NewBestValue, BestMove, BestValue)
        )
    ).
