% Entry Point
king_safety_bonus(Color, Board, Bonus) :-
    find_king(Board, Color, KR, KC),
    pawn_shield_score(Board, Color, KR, KC, ShieldScore),
    king_center_penalty(KR, KC, CenterPenalty),
    Bonus is ShieldScore - CenterPenalty.

%------------------------------------------------------------
% Pawn Shield Score: friendly pawns in front of the king
%------------------------------------------------------------

pawn_shield_score(Board, white, KR, KC, Score) :-
    RowFront is KR - 1,
    ShieldSquares = [ (RowFront, KC-1), (RowFront, KC), (RowFront, KC+1) ],
    count_pawns(Board, ShieldSquares, 'P', Count),
    Score is Count * 10.

pawn_shield_score(Board, black, KR, KC, Score) :-
    RowFront is KR + 1,
    ShieldSquares = [ (RowFront, KC-1), (RowFront, KC), (RowFront, KC+1) ],
    count_pawns(Board, ShieldSquares, 'p', Count),
    Score is Count * 10.

count_pawns(_, [], _, 0).
count_pawns(Board, [(R,C)|Rest], Pawn, Count) :-
    ( R >= 0, R < 8, C >= 0, C < 8,
      nth0(R, Board, Row), nth0(C, Row, Piece), Piece == Pawn ->
        count_pawns(Board, Rest, Pawn, RestCount),
        Count is RestCount + 1
    ;
        count_pawns(Board, Rest, Pawn, Count)
    ).

%------------------------------------------------------------
% Center Penalty: Penalize king staying near the center
%------------------------------------------------------------

king_center_penalty(R, C, Penalty) :-
    ( R >= 2, R =< 5, C >= 2, C =< 5 ->
        Penalty = 30
    ; 
        Penalty = 0
    ).
