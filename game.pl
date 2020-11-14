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
    display_game(GameState, Player).

test_game(GameState) :-
    GameState = [
        [  % Game board
            [ [],     [],     [],  [2, 4], [2, 4]],
            [ [],     [],     [],  [1],     [2, 4]],
            [ [],     [],     [1, 2, 4],  [],     []],
            [ [1, 3], [1, 3],     [],  [],     []],
            [ [1, 3], [1], [],  [],     []]
        ],
        5, % Unplayed white rings
        5, % Unplayed black rings
        3  % Shown Stack Size
    ].

play_test(Test) :-
    test_game(GameState),
    Player = white,
    move_ball(GameState, white, 4, 0, 4, 1, NewGameState, _),
    move_ball(NewGameState, white, 3, 1, 1, 3, NewGameState1, Test),
    display_game(NewGameState1, Player).
