:- use_module(library(lists)).

% Functions

ite(I, T, _) :-
    I, !, T.
ite(_, _, E) :-
    E.

clone([],[]).
clone([H|T],[H|Z]):- clone(T,Z).

% GAME:

% 0 : Nothing    : 32   : ' '
% 1 : White Ring : 9651 :
% 2 : Black Ring : 9650 :
% 3 : White Ball : 9675 :
% 4 : Black Ball : 9679 :
code(0, 32).
code(1, 9633).
code(2, 9632).
code(3, 9675).
code(4, 9679).


% If then else like
print_number_with_line(Number) :-
    Number > 10, !,
    X is Number // 10,
    Y is Number rem 10,
    digit_code(X, Code1),
    digit_code(Y, Code2),
    put_code(Code1),
    put_code(Code2).
print_number_with_line(Number) :-
    digit_code(Number, Code1),
    put_code(Code1),
    put_code(9472).

digit_code(0, 48).
digit_code(1, 49).
digit_code(2, 50).
digit_code(3, 51).
digit_code(4, 52).
digit_code(5, 53).
digit_code(6, 54).
digit_code(7, 55).
digit_code(8, 56).
digit_code(9, 57).

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
    code(1, WhiteRingCode),
    print_remaining_pieces(WhiteRings, WhiteRingCode),
    nl,
    get_black_rings(GameState, BlackRings),
    write('Unplayed black rings:'),
    code(2, BlackRingCode),
    print_remaining_pieces(BlackRings, BlackRingCode),
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
    code(1, Char),
    put_code(Char),
    nl,
    write('    Black Ring  - '),
    code(2, Char1),
    put_code(Char1),
    nl,
    write('    White Ball  - '),
    code(3, Char2),
    put_code(Char2),
    nl,
    write('    Black Ball  - '),
    code(4, Char3),
    put_code(Char3),
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
    put_char(' '),
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

print_line(Line, LineNumber) :-
    print_line_padding,
    print_line_top(Line),
    nl,
    print_line_padding,
    print_line_mid(Line, 2),
    nl,
    put_char(' '),
    put_code(LineNumber),
    put_char(' '),
    print_line_mid(Line, 1),
    nl,
    print_line_padding,
    print_line_mid(Line, 0),
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
    put_code(9472),
    put_code(9472),
    put_code(9488),
    print_line_top(Line).

print_line_mid([], _).
print_line_mid([Stack | Line], Elem) :-
    put_code(9474),
    put_char(' '),
    put_char(' '),
    print_stack(Stack, Elem),
    put_char(' '),
    put_char(' '),
    put_code(9474),
    print_line_mid(Line, Elem).

get_piece_at_level(Level, List, Elem) :-
    ite(Level is -1, last(List, Elem), nth0(Level, List, Elem)).

print_stack(Stack, Elem) :-
    get_top_elems_from_stack(Stack, Elem, Result),
    code(Result, Char),
    put_code(Char).

print_line_bot([]).
print_line_bot([Stack | Line]) :-
    length(Stack, StackLength),
    put_code(9492),
    print_number_with_line(StackLength),
    put_code(9472),
    put_code(9472),
    put_code(9472),
    put_code(9496),
    print_line_bot(Line).


get_top_elems_from_stack(ReverseStack, Amount, Result) :-
    length(ReverseStack, ReverseLength),
    ite(ReverseLength >= 3, clone(ReverseStack, AppendedStack), true),
    ite(length(ReverseStack, 2), append_zeros(ReverseStack, 1, AppendedStack), true),
    ite(length(ReverseStack, 1), append_zeros(ReverseStack, 2, AppendedStack), true),
    get_top_elements(AppendedStack, Amount, Result).

get_top_elements([], _, Result) :-
    Result = 0.
get_top_elements([H|_], 0, Result) :-
    Result = H.
get_top_elements([_|T], Number, Result) :-
    NumberNext is Number - 1,
    get_top_elements(T, NumberNext, Result).


% get_stuffed_stack(Stack, Number, Result) :-
% 
%
%
%
%
%

append_zeros(Input, 0, Result):- clone(Input, Result).
append_zeros(Input, Number, Result) :- 
    reverse(Input, Input1),
    ResultAux = [0|Input1],
    reverse(ResultAux, ResultAux2),
    NumberNext is Number - 1,
    append_zeros(ResultAux2, NumberNext, Result).