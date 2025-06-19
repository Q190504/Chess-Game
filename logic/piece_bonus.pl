%------------------------
% Piece value (normalized)
%------------------------
piece_value('P', 100).
piece_value('N', 320).
piece_value('B', 330).
piece_value('R', 500).
piece_value('Q', 900).
piece_value('K', 20000).
piece_value('p', -100).
piece_value('n', -320).
piece_value('b', -330).
piece_value('r', -500).
piece_value('q', -900).
piece_value('k', -20000).
piece_value(e, 0).

piece_bonus(Color, Board, FinalScore) :-
    flatten(Board, Flat),
    evaluate_flat(Flat, 0, RawScore),
    (
        Color = white -> FinalScore = RawScore
    ;
        FinalScore is -RawScore
    ).
    
evaluate_flat([], Acc, Acc).
evaluate_flat([P|Rest], Acc, Res) :-
    piece_value(P, V),
    Acc1 is Acc + V,
    evaluate_flat(Rest, Acc1, Res).
