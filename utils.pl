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


% https://stackoverflow.com/questions/8519203/prolog-replace-an-element-in-a-list-at-a-specified-index
% replace(OldList, Index, ElemToReplace, NewList)
% replace([], _, _, []).
replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > -1, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).


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

append_zeros(Input, 0, Result):- clone(Input, Result).
append_zeros(Input, Number, Result) :- 
    reverse(Input, Input1),
    ResultAux = [0|Input1],
    reverse(ResultAux, ResultAux2),
    NumberNext is Number - 1,
    append_zeros(ResultAux2, NumberNext, Result).

% ------------------------
%   POSITION MANIPULATION
% ------------------------

is_negative_coords([X, Y]) :-
    X =\= -1,
    Y =\= -1.

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