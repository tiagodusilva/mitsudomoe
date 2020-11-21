% :- module('mitsudomoe', ['play.pl']).
:- ensure_loaded('display.pl').
:- ensure_loaded('gameState.pl').
:- ensure_loaded('utils.pl').
:- ensure_loaded('moves.pl').
:- ensure_loaded('input.pl').

:- use_module(library(system)).


%%%GAME MODE
% 1 - H-H
% 2 - C-H
% 3 - H-C
% 4 - C-C 
mode(1, h-h).
mode(2, c-h).
mode(3, h-c).
mode(4, c-c).

play :-
    % initial(GameState),
    test_game(GameState),
    % print_title,
    % print_legend,
    read_mode(ModeNumber),
    mode(ModeNumber, Mode),
    actual_game_loop(GameState, white, Mode).


game_loop(GameState, Player, h-c) :-
    actual_game_loop(GameState, Player, c-h).
game_loop(GameState, Player, c-h) :-
    actual_game_loop(GameState, Player, h-c).
game_loop(GameState, Player, Mode) :-
    actual_game_loop(GameState, Player, Mode).


actual_game_loop(GameState, Player, Mode) :-
    % display_game(GameState, Player),
    value(GameState, Player, Value),
    write(Value),
    repeat,
    pick_move(GameState, Player, Mode, Move),
    % new_move([[4, 1], [1, 3]], [[4, 0], [1, 3]], [[[2, 2], [4, 3]]], Player, Move),
    ite(
        move(GameState, Move, NewGameState),
        true,
        (write('Impossible move!\n'), fail)
    ),
    next_player(Player, NextPlayer),
    it(
        Mode == c-c,
        sleep(2)
    ),
    game_over(NewGameState, Player, Winner),
    ite(
        Winner == none,
        game_loop(NewGameState, NextPlayer, Mode),
        ite(
            Winner == white,
            write('White won!\n'),
            write('Black won!\n')
        )
    ).


% STACK CONTENTS
% 1 : White Ring
% 2 : Black Ring
% 3 : White Ball
% 4 : Black Ball

test_game(GameState) :-
    GameState = [
        [  % Game board
            [ [],     [],     [],  [1, 3], [1]],
            [ [],     [1],     [],  [1, 3],     [1, 3]],
            [ [],     [],     [1, 2, 4],  [],     []],
            [ [1, 3], [1],     [],  [],     []],
            [ [1, 4], [1,4], [],  [2],     []]
        ],
        5, % Unplayed white rings
        5, % Unplayed black rings
        3  % Shown Stack Size
    ].

placeable(Player, Stack) :-
    last(Stack, Last),
    owns_ring(Player, Last).


play_test(Test) :-
    initial(GameState),
    valid_moves(GameState, white, ListOfMoves),
    write(ListOfMoves),
    write('\nLenght: '),
    length(ListOfMoves, Length),
    write(Length),
    member(Test, ListOfMoves).



% To check after each player finished their turn (Return Winner(white/black/none))
game_over(GameState, Player, Winner) :-
    ite(
        \+ check_enemy_cells(GameState, Player),
        ite(
            check_own_cells(GameState, Player),
            next_player(Player, Winner),
            Winner = none
        ),
        Winner = Player
    ).

%Checks for player colored balls on the enemy's starting cells
check_enemy_cells(GameState, Player) :-
    next_player(Player, Enemy),
    get_top_elem_initial_cells(GameState, Enemy, Cells),
    nth0(0, Cells, Elem1),
    nth0(1, Cells, Elem2),
    nth0(2, Cells, Elem3),
    owns_ball(Player, Elem1),
    owns_ball(Player, Elem2),
    owns_ball(Player, Elem3).


%Checks for any balls on the player initial houses
check_own_cells(GameState, Player) :-
    get_top_elem_initial_cells(GameState, Player, Cells),
    nth0(0, Cells, Elem1),
    nth0(1, Cells, Elem2),
    nth0(2, Cells, Elem3),
    is_ball(Elem1),
    is_ball(Elem2),
    is_ball(Elem3).

call_game_loop(GameState, Player, 1) :-
    game_loop_hh(GameState, Player).


pick_move(GameState, Player, h-h, Move) :-
    read_move(GameState, Player, Move).
pick_move(GameState, Player, h-c, Move) :-
    read_move(GameState, Player, Move).
pick_move(GameState, Player, c-c, Move) :-
    choose_move(GameState, Player, Level, Move).
pick_move(GameState, Player, c-h, Move) :-
    choose_move(GameState, Player, Level, Move).
