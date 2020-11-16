:- use_module(library(lists)).

% ------------------------
%   GAMESTATE DEFINITION
% ------------------------

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

next_player(black, white).
next_player(white, black).

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

is_pos_in_bounds(RowIndex, ColIndex) :-
    RowIndex >= 0,
    RowIndex < 5,
    ColIndex >= 0,
    ColIndex < 5.

% ------------------------
%    GAMESTATE GETTERS
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

get_stack(GameState, [RowIndex, ColIndex], Stack) :-
    get_row(GameState, RowIndex, Row),
    nth0(ColIndex, Row, Stack).

get_top_elem(GameState, Coords, TopElem) :-
    get_stack(GameState, Coords, Stack),
    last(Stack, TopElem).


% ------------------------
%     MOVE DEFINITION
% ------------------------

% Displace:
% from_row
% from_col
% to_row
% to_col

new_displace(FromRow, FromCol, ToRow, ToCol, [[FromRow, FromCol], [ToRow, ToCol]]).

new_displace(FromCoords, ToCoords, [FromCoords, ToCoords]).

get_displace_from_row(Displace, FromRow) :-
    nth0(0, Displace, FromCoords),
    nth0(0, FromCoords, FromRow).

get_displace_from_col(Displace, FromCol) :-
    nth0(0, Displace, FromCoords),
    nth0(1, FromCoords, FromCol).

get_displace_to_row(Displace, ToRow) :-
    nth0(1, Displace, ToCoords),
    nth0(0, ToCoords, ToRow).

get_displace_to_col(Displace, ToCol) :-
    nth0(1, Displace, ToCoords),
    nth0(1, ToCoords, ToCol).

% Move:
% ring_displace -> [-1, -1, pos, pos] if placing a ball
% ball_displace
% [balls_relocated]

new_move(RingDisplace, BallDisplace, BallRelocations, Player, [RingDisplace, BallDisplace, BallRelocations, Player]).

get_move_ring_displace(Move, RingDisplace) :-
    nth0(0, Move, RingDisplace).

get_move_ball_displace(Move, BallDisplace) :-
    nth0(1, Move, BallDisplace).

get_move_balls_relocated(Move, BallRelocations) :-
    nth0(2, Move, BallRelocations).

get_move_player(Move, Player) :-
    nth0(3, Move, Player).

% ------------------------
%         REPLACE
% ------------------------
replace_board(GameState, NewBoard, NewGameState) :-
    replace(GameState, 0, NewBoard, NewGameState).

replace_row(GameState, RowIndex, NewRow, NewGameState) :-
    get_board(GameState, Board),
    replace(Board, RowIndex, NewRow, NewBoard),
    replace_board(GameState, NewBoard, NewGameState).

replace_stack(GameState, [RowIndex, ColIndex], NewStack, NewGameState) :-
    get_row(GameState, RowIndex, Row),
    replace(Row, ColIndex, NewStack, NewRow),
    replace_row(GameState, RowIndex, NewRow, NewGameState).

replace_remaining_rings(GameState, white, NewRemainingRings, NewGameState) :-
    replace(GameState, 1, NewRemainingRings, NewGameState).

replace_remaining_rings(GameState, black, NewRemainingRings, NewGameState) :-
    replace(GameState, 2, NewRemainingRings, NewGameState).
