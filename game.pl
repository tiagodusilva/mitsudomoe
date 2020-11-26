% :- module('mitsudomoe', ['play.pl']).
:- ensure_loaded('display.pl').
:- ensure_loaded('gameState.pl').
:- ensure_loaded('utils.pl').
:- ensure_loaded('moves.pl').
:- ensure_loaded('input.pl').

:- use_module(library(system)).
:- use_module(library(random)).


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
    initial(GameState),
    print_title,
    print_legend,
    read_mode(ModeNumber),
    mode(ModeNumber, Mode),
    actual_game_loop(GameState, white, Mode, Level).


game_loop(GameState, Player, h-c, Level1-Level2) :-
    actual_game_loop(GameState, Player, c-h, Level2-Level1).
game_loop(GameState, Player, c-h, Level1-Level2) :-
    actual_game_loop(GameState, Player, h-c, Level2-Level1).
game_loop(GameState, Player, Mode, Level1-Level2) :-
    actual_game_loop(GameState, Player, Mode, Level2-Level1).


actual_game_loop(GameState, Player, Mode, Level) :-
    display_game(GameState, Player),
    repeat,
    pick_move(GameState, Player, Mode, Move, Level),
    % new_move([[4, 1], [1, 3]], [[4, 0], [1, 3]], [[[2, 2], [4, 3]]], Player, Move),
    try_move(GameState, Move, NextGameState),
    next_player(Player, NextPlayer),
    %Used to check the moves of the computer
    bot_sleep(Mode),
    game_over(NextGameState, Player, Winner),
    handle_winner(NextGameState, NextPlayer, Mode, Winner, Level).


bot_sleep(c-c) :-
    % true.
    sleep(1.5).
bot_sleep(_).

try_move(GameState, Move, NewGameState) :-
    move(GameState, Move, NewGameState).
try_move(_, _, _) :-
    write('Impossible move!\n'),
    fail.


handle_winner(NextGameState, NextPlayer, Mode, none, Level) :-
    game_loop(NextGameState, NextPlayer, Mode, Level).
handle_winner(GameState, NextPlayer, _, white) :-
    display_game(GameState, NextPlayer),
    write('White won!\n').
handle_winner(GameState, NextPlayer, _, black) :-
    display_game(GameState, NextPlayer),
    write('Black won!\n').


% STACK CONTENTS
% 1 : White Ring
% 2 : Black Ring
% 3 : White Ball
% 4 : Black Ball

% test_game(GameState) :-
%     GameState = [
%         [  % Game board
%             [ [],     [],     [],  [2, 4], [2]],
%             [ [],     [],     [],  [2, 4],     [2, 4]],
%             [ [],     [],     [1, 3],  [],     []],
%             [ [1, 3], [1],     [],  [],     []],
%             [ [1], [1, 3], [],  [],     []]
%         ],
%         3, % Unplayed white rings
%         4, % Unplayed black rings
%         3  % Shown Stack Size
%     ].


test_game(GameState) :-
    GameState = [
        [  % Game board
            [ [],     [],     [],  [2, 4], [2, 4]],
            [ [],     [],     [],  [],     [2, 4]],
            [ [],     [],     [],  [],     []],
            [ [1, 3], [], [],  [],     []],
            [ [1, 3], [1, 3], [],  [],     []]
        ],
        4, % Unplayed white rings
        4, % Unplayed black rings
        3  % Shown Stack Size
    ].


play_test(Test) :-
    % Move = [[[-1, -1], [3, 1]], [[1, 3], [3, 1]], [[[2, 2], [4, 0]]], black],
    % move(GameState, Move, _).
    test_game(GameState),
    valid_moves(GameState, white, ListOfMoves),
    nth0(0, ListOfMoves, Move),
    % lambda_evaluate_move_d2(GameState, white, Move, Test),
    min_map(ListOfMoves, [lambda_evaluate_move_d2, GameState, Player], Test),
    write(Test),
    nl.
    % valid_moves(GameState, black, ListOfMoves),
    % write(ListOfMoves),
    % write('\nLenght: '),
    % length(ListOfMoves, Length),
    % write(Length),
    % !, member(Test, ListOfMoves).



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

% pick_move(GameState, Player, Mode, Move, Level)
pick_move(GameState, Player, h-h, Move, _) :-
    read_move(GameState, Player, Move).
pick_move(GameState, Player, h-c, Move, _) :-
    read_move(GameState, Player, Move).
pick_move(GameState, Player, c-c, Move, Level-_) :-
    choose_move(GameState, Player, Level, Move).
pick_move(GameState, Player, c-h, Move, Level-_) :-
    choose_move(GameState, Player, Level, Move).


choose_move(GameState, Player, random, Move) :-
    valid_moves(GameState, Player, ListOfMoves),
    random_member(Move, ListOfMoves).

choose_move(GameState, Player, smart, Move) :-
    valid_moves(GameState, Player, ListOfMoves),
    % write('Possible Moves: '),
    % length(ListOfMoves, NumMoves),
    % write(NumMoves),
    % nl,
    % get best move
    min_map(ListOfMoves, [lambda_evaluate_move, GameState, Player], Move).


lambda_evaluate_move(GameState, Player, Move, Value) :-
    move(GameState, Move, NewGameState),
    value(NewGameState, Player, ValuePlayer),
    next_player(Player, NextPlayer),
    value(NewGameState, NextPlayer, ValueEnemy),
    random(-0.5, 0.5, Rand),
    Value is ValuePlayer + ValueEnemy * 0.5 + Rand. % Rand is used to make the AI not deterministic
    % Value is ValuePlayer + ValueEnemy * 0.5.


% % % % % % DEPTH 2 OPTIONAL STUFF
choose_move(GameState, Player, smart2, Move) :-
    valid_moves(GameState, Player, ListOfMoves),
    % get best move
    min_map(ListOfMoves, [lambda_evaluate_move_d2, GameState, Player], Move).

%Used to check if a move will make the enemy win on his best play, otherwise calculate value normally
value_calc(Winner, _, Winner, _, Value) :-
    Value is 5000.
value_calc(_, GameState, Player, D1Value, Value) :-
    next_player(Player, NextPlayer),
    choose_move_V(GameState, NextPlayer, D2Value),
    Value is D1Value + 0.2 * D2Value.

lambda_evaluate_move_d2(GameState, Player, Move, Value) :-
    %Evaluate first play
    value(GameState, Player, D1Value),
    %Make first move
    move(GameState, Move, NewGameState),
    %get best move of enemy
    next_player(Player, Enemy),
    choose_move(NewGameState, Enemy, smart, NextMove),
    move(NewGameState, NextMove, NextGameState),
    game_over(GameState, Enemy, Winner),
    % Calculation of the value if the game didnt end on the enemy move
    value_calc(Winner, NextGameState, Enemy, D1Value, Value).

%Returns the value for the best move (Used on depth 2)
choose_move_V(GameState, Player, Value) :-
    valid_moves(GameState, Player, ListOfMoves),
    % get best move
    min_map(ListOfMoves, [lambda_evaluate_move, GameState, Player], Move),
    move(GameState, Move, NewGameState),
    value(NewGameState, Player, Value).


    