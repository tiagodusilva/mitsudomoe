% :- module('mitsudomoe', ['play.pl']).
:- ensure_loaded('display.pl').
:- ensure_loaded('gameState.pl').
:- ensure_loaded('utils.pl').

play :-
    initial(GameState),
    print_title,
    print_legend,
    display_game(GameState, Player).

