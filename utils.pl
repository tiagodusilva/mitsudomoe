:- use_module(library(lists)).

ite(I, T, _) :-
    I, !, T.
ite(_, _, E) :-
    E.


clone([],[]).
clone([H|T],[H|Z]):- clone(T,Z).


append_zeros(Input, 0, Result):- clone(Input, Result).
append_zeros(Input, Number, Result) :- 
    reverse(Input, Input1),
    ResultAux = [0|Input1],
    reverse(ResultAux, ResultAux2),
    NumberNext is Number - 1,
    append_zeros(ResultAux2, NumberNext, Result).

