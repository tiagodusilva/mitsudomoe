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
    game_over(NewGameState, Player, Winner),
    ite(
        Winner == none,
        game_loop(NewGameState, NextPlayer),
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
    test_game(GameState),
    Player = white,
    PredList = [placeable, Player],
    get_stacks_if(GameState, PredList, Test).

%To check after each player finished their turn (Return Winner(white/black/none))
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


    

