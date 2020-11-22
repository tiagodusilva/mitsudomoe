read_char(Char) :-
    read(Char),
    atom(Char).

read_number(Number) :-
    read(Number),
    number(Number).

read_move(GameState, Player, Move) :-
    % Move or Place Ring
    write('Would you like to move or place a new ring? [m / p] \n'),
    read_char(RingType),
    it(
        (RingType \== 'm' , RingType \== 'p'),
        (
            write('Invalid operation :(\n'),
            fail
        )
    ),
    ite(
        RingType == 'm',
        (read_displace('MOVE RING\n', RingDisplace)),
        (
            % TODO: Check if you still have rings to place
            read_coord_pair('PLACE NEW RING\n', PlaceRingCoords),
            new_displace([-1, -1], PlaceRingCoords, RingDisplace)
        )
    ),
    % Move own Ball
    read_displace('MOVE BALL\n', BallDisplace),
    % RingDisplace = [[-1, -1], [0, 0]],
    % BallDisplace = [[4, 0], [1, 3]],
    move_ball(GameState, Player, BallDisplace, NewGameState, BallsToDisplace),
    next_player(Player, DisplacedPlayer),
    read_displacements(NewGameState, DisplacedPlayer, BallsToDisplace, [], FinalDisplacements),
    

    new_move(RingDisplace, BallDisplace, FinalDisplacements, Player, Move).


read_mode(Mode) :-
    print_mode_select,
    read_number(Mode).

letter_to_col('a', 0).
letter_to_col('b', 1).
letter_to_col('c', 2).
letter_to_col('d', 3).
letter_to_col('e', 4).
letter_to_col('f', 5).
letter_to_col('g', 6).
letter_to_col('h', 7).
letter_to_col('i', 8).
letter_to_col('j', 9).
letter_to_col('k', 10).
letter_to_col('l', 11).
letter_to_col('m', 12).
letter_to_col('n', 13).
letter_to_col('o', 14).
letter_to_col('p', 15).
letter_to_col('q', 16).
letter_to_col('r', 17).
letter_to_col('s', 18).
letter_to_col('t', 19).
letter_to_col('u', 20).
letter_to_col('v', 21).
letter_to_col('w', 22).
letter_to_col('x', 23).
letter_to_col('y', 24).
letter_to_col('z', 25).

% Displays custom Message and reads a pair of coordinates
read_coord_pair(Message, Coords) :-
    write(Message),
    write('Please insert row [0 ~ 4] '),
    read_number(Row),
    write('Please insert col [a ~ e] '),
    read_char(ColLetter),
    letter_to_col(ColLetter, Col),
    number(Col),
    Coords = [Row, Col].

% Displays a costum message and reads a displacement(from/to)
read_displace(Message, Displace) :-
    write(Message),
    read_coord_pair('Which piece would you like to move?\n', FromCoords),
    read_coord_pair('Where would you like to move it to?\n', ToCoords),
    new_displace(FromCoords, ToCoords, Displace).

%TODO change length comparation
%Reads the displacements after a vault
read_displacements(_, _, [], Displacements, Displacements).
read_displacements(GameState, Player, BallsToDisplace, Displacements, FinalDisplacements) :-
    read_displace('DISPLACE ENEMY BALLS\n', Displace),
    % Displace = [[2, 2], [4, 3]],
    new_displace(FromCoords, _, Displace),
    delete(BallsToDisplace, FromCoords, NewBallsToDisplace),
    \+ same_length(BallsToDisplace, NewBallsToDisplace),
    displace_ball(GameState, Player, Displace, NewGameState),
    append(Displacements, [Displace], NewDisplacements),
    read_displacements(NewGameState, Player, NewBallsToDisplace, NewDisplacements, FinalDisplacements).






