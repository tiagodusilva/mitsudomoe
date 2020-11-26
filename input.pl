read_code(Code, Possible) :-
    get_code(Code),
    write(Code), nl,
    member(Code, Possible).

read_char(Char, Possible) :-
    get_char(Char),
    member(Char, Possible).

% Inclusive on both ends to facilitate A-Z (but uses their ASCII codes)
% read_code_range(Char, ['A'-'Z', 'a'-'z']) :-
read_code_range(Code, Ranges) :-
    get_code(Code),
    is_code_in_range(Code, Ranges).

is_code_in_range(_, []) :-
    fail.
is_code_in_range(Code, [RangeStart-RangeEnd | _]) :-
    Code >= RangeStart,
    Code =< RangeEnd.
is_code_in_range(Code, [_ | T]) :-
    is_code_in_range(Code, T).

read_digit(Digit) :-
    read_code_range(Code, [48-57]),
    Digit is Code - 48.

read_lowercase_code(Code) :-
    read_code_range(Code, [97-122]).
read_lowercase(Letter) :-
    read_lowercase_code(Code),
    char_code(Letter, Code).

read_uppercase_code(Code) :-
    read_code_range(Code, [65-90]).
read_uppercase(Letter) :-
    read_uppercase_code(Code),
    char_code(Letter, Code).

read_letter_code(Code) :-
    read_code_range(Code, [65-90, 97-122]).
read_letter(Letter) :-
    read_letter_code(Code),
    char_code(Letter, Code).

% Structured oddly to allow either A2 or 2B
read_coord(Coord) :-
    read_code_range(Code, [65-90, 97-122, 48-57]),
    read_coord_part2(Code, Number, Col),
    Coord = [Number, Col].
% A2 format
read_coord_part2(Code, Number, Col) :-
    code_to_col(Code, Col),
    Col < 5,
    read_digit(Number),
    Number < 5.
% 2b format
read_coord_part2(Code, Number, Col) :-
    number_code_to_digit(Code, Number),
    Number < 5,
    read_letter_code(Code2),
    code_to_col(Code2, Col),
    Col < 5.

% Converts ASCII code of a digit to a number
number_code_to_digit(Code, Number) :-
    Code >= 48,
    Code =< 57,
    Number is Code - 48.
% Converts ASCII code of an uppercase letter to the respective collumn
code_to_col(Code, Col) :-
    Code >= 65,
    Code =< 90,
    Col is Code - 65.
% Converts ASCII code of an lowercase letter to the respective collumn
code_to_col(Code, Col) :-
    Code >= 97,
    Code =< 122,
    Col is Code - 97.


write_player_move(white) :-
    write('--------------------'), nl,
    write('|   WHITE\'S TURN    |'), nl,
    write('--------------------'), nl.
write_player_move(black) :-
    write('--------------------'), nl,
    write('|   BLACK\'S TURN    |'), nl,
    write('--------------------'), nl.


read_ring('m', GameState, Player, RingDisplace, NewGameState) :-
    read_displace('MOVE RING', RingDisplace),
    move_ring(GameState, Player, RingDisplace, NewGameState).
read_ring('p', GameState, Player, RingDisplace, NewGameState) :-
    % TODO: Check if you still have rings to place
    write('Select where to place: '),
    read_coord(PlaceRingCoords), skip_line,
    new_displace([-1, -1], PlaceRingCoords, RingDisplace),
    place_new_ring(GameState, Player, PlaceRingCoords, NewGameState).

read_move(GameState, Player, Move) :-
    nl,
    write_player_move(Player), nl,
    repeat,
    % Move or Place Ring
    write('---- Ring Phase ----'), nl,
    write('[M]ove or [P]lace a ring: '),
    read_char(RingType, ['m', 'p']), skip_line,
    read_ring(RingType, GameState, Player, RingDisplace, AfterRingGameState),
    % Move own Ball
    write('---- Ball Phase ----'), nl,
    read_displace(BallDisplace),
    move_ball(AfterRingGameState, Player, BallDisplace, NewGameState, BallsToDisplace),
    next_player(Player, DisplacedPlayer),
    % Move enemy balls if needed
    read_displacements(NewGameState, DisplacedPlayer, BallsToDisplace, FinalDisplacements),
    new_move(RingDisplace, BallDisplace, FinalDisplacements, Player, Move).


read_mode(Mode, Max) :-
    print_mode_select,
    repeat,
    read_digit(Mode),
    skip_line,
    Mode =< Max.


% Displays a costum message and reads a displacement(from/to)
read_displace(Displace) :-
    write('Select piece to move: '),
    read_coord(FromCoords), skip_line,
    write('Select where to: '),
    read_coord(ToCoords), skip_line,
    new_displace(FromCoords, ToCoords, Displace).


read_displacements_wrapper(_, _, [], []).
read_displacements_wrapper(GameState, Player, BallsToDisplace, Displacements) :-
    write('---- Enemy Ball Relocation Phase ----'), nl,
    read_displacements(GameState, Player, BallsToDisplace, Displacements).


% Reads the displacements after a vault
read_displacements(_, _, [], []).
read_displacements(GameState, Player, BallsToDisplace, [Displacement | T]) :-
    write('Balls left to relocate: '),
    write(BallsToDisplace), nl,
    read_displace(Displacement),
    new_displace(FromCoords, _, Displacement),
    delete(BallsToDisplace, FromCoords, NewBallsToDisplace),
    \+ same_length(BallsToDisplace, NewBallsToDisplace),
    displace_ball(GameState, Player, Displacement, NewGameState),
    read_displacements(NewGameState, Player, NewBallsToDisplace, T).



get_level(s, smart).
get_level(r, random).

% Reads the ai level
read_level(h-h, human-human).
read_level(h-c, human-Level) :-
    read_level(c-h, Level-human).
read_level(c-h, Level-human) :-
    write('Choose AI level [s]mart or [r]andom:'),
    %TODO
    read(LevelLetter),
    get_level(LevelLetter, Level).
read_level(c-c, Level1-Level2) :-
    write('Choose AI 1 level [s]mart or [r]andom:'),
    read(Level1Letter),
    get_level(Level1Letter, Level),
    write('Choose AI 2 level [s]mart or [r]andom:'),
    read(Level2Letter),
    get_level(Level2Letter, Level).


