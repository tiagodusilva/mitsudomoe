:- use_module(library(lists)).

initial(GameState) :-
    GameState = [
        [  % Game board
            [ [],     [],     [],  [2, 4], [2, 4]],
            [ [],     [],     [],  [],     [2, 4]],
            [ [],     [],     [],  [],     []],
            [ [1, 3], [],     [],  [],     []],
            [ [1, 3], [1, 3], [],  [],     []]
        ],
        5, % Unplayed white rings
        5, % Unplayed black rings
        3  % Shown Stack Size
    ].

mid_game(GameState) :-
    GameState = [
        [  % Game board
            [ [],     [],     [],        [2, 1], [1, 3]],
            [ [],     [1, 3], [],        [2, 4], [2, 4]],
            [ [],     [1],    [2, 1, 3], [2],    [1]],
            [ [2, 4], [2, 1], [2],       [],     []],
            [ [],     [],     [],        [],     []]
        ],
        1, % Unplayed white rings
        0, % Unplayed black rings
        4  % Shown Stack Size
    ].

end_game(GameState) :-
    GameState = [
        [  % Game board
            [ [],     [],     [],     [1, 2, 1, 3], [1, 3]],
            [ [],     [],     [],     [2, 4],       [2, 1, 3]],
            [ [],     [1],    [2, 1], [],           []],
            [ [2, 4], [2, 1], [2, 1], [],           []],
            [ [2, 4], [],     [],     [],           []]
        ],
        0, % Unplayed white rings
        0, % Unplayed black rings
        5  % Shown Stack Size
    ].


% PLAYER:
% black
% white

% STACK CONTENTS
% 1 : White Ring
% 2 : Black Ring
% 3 : White Ball
% 4 : Black Ball

owns_ring(white, 1).
owns_ring(black, 2).
owns_ball(white, 3).
owns_ball(black, 4).

is_ring(1).
is_ring(2).
is_ball(3).
is_ball(4).

% ------------------------
%         GETTERS
% ------------------------
get_board(GameState, Board) :-
    GameState = [Board | _].

get_white_rings(GameState, WhiteRings) :-
    nth0(1, GameState, WhiteRings).

get_black_rings(GameState, BlackRings) :-
    nth0(2, GameState, BlackRings).

get_remaining_player_rings(GameState, white, RemainingRings) :-
    get_white_rings(GameState, RemainingRings).

get_remaining_player_rings(GameState, black, RemainingRings) :-
    get_black_rings(GameState, RemainingRings).

get_shown_stack_size(GameState, ShownStackSize) :-
    nth0(3, GameState, ShownStackSize).

get_row(GameState, RowIndex, Row) :-
    get_board(GameState, Board),
    nth0(RowIndex, Board, Row).

get_stack(GameState, RowIndex, ColIndex, Stack) :-
    get_row(GameState, RowIndex, Row),
    nth0(ColIndex, Row, Stack).


% ------------------------
%         REPLACE
% ------------------------
replace_board(GameState, NewBoard, NewGameState) :-
    replace(GameState, 0, NewBoard, NewGameState).

replace_row(GameState, RowIndex, NewRow, NewGameState) :-
    get_board(GameState, Board),
    replace(Board, RowIndex, NewRow, NewBoard),
    replace_board(GameState, NewBoard, NewGameState).

replace_stack(GameState, RowIndex, ColIndex, NewStack, NewGameState) :-
    get_row(GameState, RowIndex, Row),
    replace(Row, ColIndex, NewStack, NewRow),
    replace_row(GameState, RowIndex, NewRow, NewGameState).

replace_remaining_rings(GameState, white, NewRemainingRings, NewGameState) :-
    replace(GameState, 1, NewRemainingRings, NewGameState).

replace_remaining_rings(GameState, black, NewRemainingRings, NewGameState) :-
    replace(GameState, 2, NewRemainingRings, NewGameState).
