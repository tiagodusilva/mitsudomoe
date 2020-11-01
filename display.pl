:- use_module(library(lists)).

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


% GameState, Player
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
    % nl,
    % print_player(Player),
    nl.

% boundary(vert, 9474).
% boundary(hor, 9472).
% boundary(top_left, 9484).
% boundary(top_right, 9488).
% boundary(bot_left, 9492).
% boundary(bot_right, 9496).

% print_player(Player).

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
    print_line_mid(Line, 0),
    nl,
    put_char(' '),
    put_code(LineNumber),
    put_char(' '),
    print_line_mid(Line, 1),
    nl,
    print_line_padding,
    print_line_mid(Line, 2),
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
    get_stuffed_elem_from_end0(Stack, Elem, 3, Result),
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


get_stuffed_elem_from_end0(Stack, Position, StuffLength, Result) :-
    length(Stack, Length),
    ite(
        Length < StuffLength,
        % Pos is  Length - Position + (StuffLength - Length), simplified below
        Pos is  StuffLength - Position,
        Pos is Length - Position
    ),
    ite((Pos =< Length, Pos > 0), nth1(Pos, Stack, Result), Result is 0).
