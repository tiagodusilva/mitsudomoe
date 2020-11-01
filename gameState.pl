:- use_module(library(lists)).

% 1 : White Ring : 9651 :
% 2 : Black Ring : 9650 :
% 3 : White Ball : 9675 :
% 4 : Black Ball : 9679 :

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

mid_game(GameState) :-
    GameState = [
        [  % Game board (yeah, really)
            [ [],     [],     [],  [2, 1], [1, 3]],
            [ [],     [1, 3],     [],  [2, 4],     [2, 4]],
            [ [],     [1],     [2, 1, 3],  [2],     [1]],
            [ [2, 4], [2, 1],     [2],  [],     []],
            [ [], [], [],  [],     []]
        ],
        1, % Unplayed white rings
        0  % Unplayed black rings
    ].

end_game(GameState) :-
    GameState = [
        [  % Game board (yeah, really)
            [ [],     [],     [],  [1, 2, 1, 3], [1, 3]],
            [ [],     [],     [],  [2, 4],     [2, 1, 3]],
            [ [],     [1],     [2, 1],  [],     []],
            [ [2, 4], [2, 1],     [2, 1],  [],     []],
            [ [2, 4], [], [],  [],     []]
        ],
        0, % Unplayed white rings
        0  % Unplayed black rings
    ].

get_board(GameState, Board) :-
    GameState = [Board | _].

get_white_rings(GameState, WhiteRings) :-
    nth0(1, GameState, WhiteRings).

get_black_rings(GameState, BlackRings) :-
    nth0(2, GameState, BlackRings).

