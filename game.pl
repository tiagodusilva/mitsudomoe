% :- module('mitsudomoe', ['play.pl']).
:- ensure_loaded('display.pl').
:- ensure_loaded('gameState.pl').
:- ensure_loaded('utils.pl').
:- ensure_loaded('moves.pl').
:- ensure_loaded('input.pl').

play :-
    % initial(GameState),
    test_game(GameState),
    % print_title,
    % print_legend,
    Player = white,
    game_loop(GameState, Player).
   

game_loop(GameState, Player) :-
    display_game(GameState, Player),
    repeat,
    read_move(GameState, Player, Move),
    % new_move([[4, 1], [1, 3]], [[4, 0], [1, 3]], [[[2, 2], [4, 3]]], Player, Move),
    ite(
        move(GameState, Move, NewGameState),
        true,
        (write('Impossible move!\n'), fail)
    ),
    next_player(Player, NextPlayer),
    game_loop(NewGameState, NextPlayer).

test_game(GameState) :-
    GameState = [
        [  % Game board
            [ [],     [],     [],  [2, 4], [2, 4]],
            [ [],     [],     [],  [1],     [2, 4]],
            [ [],     [],     [1, 2, 4],  [],     []],
            [ [1, 3], [1, 3],     [],  [],     []],
            [ [1, 3], [1], [],  [2],     []]
        ],
        5, % Unplayed white rings
        5, % Unplayed black rings
        3  % Shown Stack Size
    ].

play_test(Test) :-
    test_game(GameState),
    Player = white,
    read_move(GameState, Player, Test).
