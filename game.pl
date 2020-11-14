% :- module('mitsudomoe', ['play.pl']).
:- ensure_loaded('display.pl').
:- ensure_loaded('gameState.pl').
:- ensure_loaded('utils.pl').
:- ensure_loaded('moves.pl').

play :-
    initial(GameState),
    print_title,
    print_legend,
    Player = white,
    place_new_ring(GameState, Player, 3, 0, NewGameState),
    display_game(NewGameState, Player).

play_test(Test) :-
    initial(GameState),
    Player = white,
    place_new_ring(GameState, Player, 2, 3, NewGameState),
    get_stack(NewGameState, 3, 0, Test),
    display_game(NewGameState, Player).
