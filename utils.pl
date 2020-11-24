:- use_module(library(lists)).

% If then else
ite(I, T, _) :-
    I, !, T.
ite(_, _, E) :-
    E.

% If then
it(I, T) :- ite(I, T, true).

% Clones a list
clone([],[]).
clone([H|T],[H|Z]):- clone(T,Z).


% replace(OldList, Index, ElemToReplace, NewList)
replace([_|T], 0, ElemToReplace, [ElemToReplace|T]).
replace([H|T1], Index, ElemToReplace, [H|T2]) :-
    Index > -1,
    NextIndex is Index - 1,
    replace(T1, NextIndex, ElemToReplace, T2).



% http://kti.ms.mff.cuni.cz/~bartak/prolog/combinatorics.html
% Generates a combination
comb(0,_,[]).
comb(N,[X|T],[X|Comb]):- N>0,N1 is N-1,comb(N1,T,Comb).
comb(N,[_|T],Comb):- N>0,comb(N,T,Comb).

% Using findall we can create a list with ALL valid combinations
combinations(N, Input, Output) :-
    findall(X, comb(N, Input, X), Output).

combinations_except(N, Input, Except, Output) :-
    findall(X, (comb(N, Input, X), X \= Except), Output).



min_map(List, Predicate, Min) :-
    min_map(List, Predicate, Min, _, 99999).

min_map([], _, Min, Min, _).
min_map([H | List], Predicate, Min, CurBest, CurBestVal) :-
    append(Predicate, [H, Value], PredList),
    Pred =.. PredList,
    Pred,
    min_map_is_better(H, Value, CurBest, CurBestVal, NextBest, NextBestVal),
    min_map(List, Predicate, Min, NextBest, NextBestVal).

min_map_is_better(Cur, Value, _, CurBestVal, NextBest, NextBestVal) :-
    Value < CurBestVal,
    NextBest = Cur,
    NextBestVal is Value.
min_map_is_better(_, _, CurBest, CurBestVal, NextBest, NextBestVal) :-
    NextBest = CurBest,
    NextBestVal is CurBestVal.

% ------------------------
%   POSITION MANIPULATION
% ------------------------

add_coords([X1, Y1], [X2, Y2], Result) :-
    Rx is X1 + X2,
    Ry is Y1 + Y2,
    Result = [Rx, Ry].

is_pos_different([CoordsA, CoordsB]) :- is_pos_different(CoordsA, CoordsB).
is_pos_different([RowIndexA, ColIndexA], [RowIndexB, ColIndexB]) :-
    % Either Row or Col must be different
    % With DeMorgan: not (same row AND same col) 
    \+ (
        RowIndexA =:= RowIndexB,
        ColIndexA =:= ColIndexB
    ).

% Adjacent is vertical horizontal or diagonal
is_adjacent([FromCoords, ToCoords]) :- is_adjacent(FromCoords, ToCoords).
is_adjacent([FromRowIndex, FromColIndex], [ToRowIndex, ToColIndex]) :-
    RowAdj is abs(FromRowIndex - ToRowIndex),
    ColAdj is abs(FromColIndex - ToColIndex),
    % Same cell
    (RowAdj + ColAdj) =\= 0,
    RowAdj =< 1,
    ColAdj =< 1.


% Given a value, set it to -1, 0 or 1
normalize_delta(0, 0).
normalize_delta(Value, Delta) :-
    Value < 0,
    Delta is -1.
normalize_delta(_, Delta) :-
    Delta is 1.


% Returns a unitary "vector" given two cells
get_direction([[FromRowIndex, FromColIndex], [ToRowIndex, ToColIndex]], [DeltaRow, DeltaCol]) :-
    RowAdj is (ToRowIndex - FromRowIndex),
    ColAdj is (ToColIndex - FromColIndex),
    % Verify if perfect diagonal, horizontal or vertical (in order)
    \+ (
        \+ (
            RowAdj =\= 0,
            ColAdj =\= 0,
            % Only true if perfect diagonal
            abs(RowAdj) =:= abs(ColAdj)
        ),
        \+ (
            RowAdj =\= 0,
            ColAdj =:= 0
        ),
        \+ (
            RowAdj =:= 0,
            ColAdj =\= 0
        )
    ),
    normalize_delta(RowAdj, DeltaRow),
    normalize_delta(ColAdj, DeltaCol).

nth0_from_end(Index, List, Elem) :-
    reverse(List, RevList),
    nth0(Index, RevList, Elem).

dist([X1, Y1], [X2, Y2], Result) :-
    Result is sqrt(exp(X1 - X2, 2) + exp(Y1 - Y2, 2)).

avg(List, Result) :-
    length(List, Len),
    sumlist(List, Sum),
    Result is Sum / Len.