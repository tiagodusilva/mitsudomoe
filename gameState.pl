:- use_module(library(lists)).

initial(GameState) :-
    GameState = [
        [  % Game board (yeah, really)
            [ [],     [],     [],  [2, 4], [2, 4]],
            [ [],     [],     [],  [],     [2, 4]],
            [ [],     [],     [],  [],     []],
            [ [1, 3], [],     [],  [],     []],
            [ [1, 3], [1, 3], [],  [],     []]
        ],
        5, % Unplayed white rings
        5  % Unplayed black rings
    ].

get_board(GameState, Board) :-
    GameState = [Board | _].

get_white_rings(GameState, WhiteRings) :-
    nth0(1, GameState, WhiteRings).

get_black_rings(GameState, BlackRings) :-
    nth0(2, GameState, BlackRings).

