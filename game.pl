:- use_module(library(lists)).

% Functions

ite(I, T, _) :-
    I, !, T.
ite(_, _, E) :-
    E.

% GAME:

% 1 : White Ring :
% 2 : Black Ring : 
% 3 : White Ball : 
% 4 : Black Ball : 
code(1, 9651).
code(2, 9650).
code(3, 9675).
code(4, 9679).


play :-
    initial(GameState),
    print_title,
    print_legend,
    display_game(GameState, Player).


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

display_game(GameState, Player) :-
    get_board(GameState, Board),
    nl,
    print_line_padding,
    print_top_letters(Board, 65),  % 65 is 'a'
    nl,
    print_board(Board, 48),  % 48 is '0'
    nl,
    nl,
    get_white_rings(GameState, WhiteRings),
    write('Unplayed white rings:'),
    print_remaining_pieces(WhiteRings, 9651),
    nl,
    get_black_rings(GameState, BlackRings),
    write('Unplayed black rings:'),
    print_remaining_pieces(BlackRings, 9650),
    nl.

% boundary(vert, 9474).
% boundary(hor, 9472).
% boundary(top_left, 9484).
% boundary(top_right, 9488).
% boundary(bot_left, 9492).
% boundary(bot_right, 9496).

print_title :-
    nl,nl,
    write(' __  __ _ _                 _ '), nl,
    write('|  \\/  (_) |               | |'), nl,
    write('| \\  / |_| |_ ___ _   _  __| | ___  _ __ ___   ___   ___'), nl,
    write('| |\\/| | | __/ __| | | |/ _` |/ _ \\| \'_ ` _ \\ / _ \\ / _ \\'), nl,
    write('| |  | | | |_\\__ \\ |_| | (_| | (_) | | | | | | (_) |  __/'), nl,
    write('|_|  |_|_|\\__|___/\\__,_|\\__,_|\\___/|_| |_| |_|\\___/ \\___|'),
    nl,
    nl.

print_legend :-
    nl,
    write('  Legend (only shown once):'),
    nl,
    write('    White Ring  - '),
    put_code(9651),
    nl,
    write('    Black Ring  - '),
    put_code(9650),
    nl,
    write('    White Ball  - '),
    put_code(9675),
    nl,
    write('    Black Ball  - '),
    put_code(9679),
    nl,
    nl.

print_remaining_pieces(0, _).
print_remaining_pieces(NumPieces, PieceCode) :-
    NextPieces is NumPieces - 1,
    put_char(' '),
    put_code(PieceCode),
    print_remaining_pieces(NextPieces, PieceCode).

print_top_letters([], _).
print_top_letters([_ | Board], Char) :-
    put_char(' '),
    put_code(Char),
    put_char(' '),
    put_char(' '),
    put_char(' '),
    NextChar is Char + 1,
    print_top_letters(Board, NextChar).


print_board([], _).
print_board([Line | Board], Number) :-
    print_line(Line, Number),
    NextNumber is Number + 1,
    print_board(Board, NextNumber).

print_line_padding :-
    put_char(' '),
    put_char(' '),
    put_char(' ').

print_line(Line, Number) :-
    print_line_padding,
    print_line_top(Line),
    nl,
    put_char(' '),
    put_code(Number),
    put_char(' '),
    print_line_mid(Line),
    nl,
    print_line_padding,
    print_line_bot(Line),
    nl.

print_line_top([]).
print_line_top([_ | Line]) :-
    put_code(9484),
    put_code(9472),
    put_code(9472),
    put_code(9472),
    put_code(9488),
    print_line_top(Line).

print_line_mid([]).
print_line_mid([Stack | Line]) :-
    put_code(9474),
    put_char(' '),
    print_stack(Stack),
    put_char(' '),
    put_code(9474),
    print_line_mid(Line).

get_piece_at_level(Level, List, Elem) :-
    ite(Level is -1, last(List, Elem), nth0(Level, List, Elem)).

print_stack([]) :-
    put_char(' ').
print_stack(Stack) :-
    last(Stack, Last),
    code(Last, Char),
    put_code(Char).

print_line_bot([]).
print_line_bot([_ | Line]) :-
    put_code(9492),
    put_code(9472),
    put_code(9472),
    put_code(9472),
    put_code(9496),
    print_line_bot(Line).

