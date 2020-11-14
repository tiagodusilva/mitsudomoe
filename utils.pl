:- use_module(library(lists)).

ite(I, T, _) :-
    I, !, T.
ite(_, _, E) :-
    E.

clone([],[]).
clone([H|T],[H|Z]):- clone(T,Z).


% https://stackoverflow.com/questions/8519203/prolog-replace-an-element-in-a-list-at-a-specified-index
% replace(OldList, Index, ElemToReplace, NewList)
% replace([], _, _, []).
replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > -1, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).


append_zeros(Input, 0, Result):- clone(Input, Result).
append_zeros(Input, Number, Result) :- 
    reverse(Input, Input1),
    ResultAux = [0|Input1],
    reverse(ResultAux, ResultAux2),
    NumberNext is Number - 1,
    append_zeros(ResultAux2, NumberNext, Result).

