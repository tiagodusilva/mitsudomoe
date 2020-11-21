:- use_module(library(lists)).

% reduce_remaining_rings(+GameState, +Player, -NewGameState)
% Decreases the amount of rings outside of play of Player after placement on the board
reduce_remaining_rings(GameState, Player, NewGameState) :-
    get_remaining_player_rings(GameState, Player, Rings),
    Rings > 0,
    NewRings is Rings - 1,
    replace_remaining_rings(GameState, Player, NewRings, NewGameState).

% can_ring_be_placed(+Stack)
% Checks if a ring can be placed on top of Stack(checks if it is empty or has a ring on top)
can_ring_be_placed([]).
can_ring_be_placed(Stack) :-
    last(Stack, LastElem),
    is_ring(LastElem).

% can_ball_be_placed(+Stack, +Player)
% Checks if a ball can be placed on top of Stack (checks for the excistance on a ring of the same player)
can_ball_be_placed(Stack, Player) :-
    last(Stack, LastElem),
    owns_ring(Player, LastElem).

% ------------------------
%      RING MOVEMENT
% ------------------------
% place_ring(+GameState, +Player, +Coords, -NewGameState)
% Places a Ring from Player at Cords
place_ring(GameState, Player, Coords, NewGameState) :-
    get_stack(GameState, Coords, Stack),
    can_ring_be_placed(Stack),
    owns_ring(Player, RingCode),
    append(Stack, [RingCode], NewStack),
    replace_stack(GameState, Coords, NewStack, NewGameState).

% place_new_ring(+GameState, +Player, +Coords, -NewGameState)
% Places a new Ring from Player at Cords
place_new_ring(GameState, Player, Coords, NewGameState) :-
    % Does the player still have rings to place?
    get_remaining_player_rings(GameState, Player, Rings),
    Rings > 0,
    place_ring(GameState, Player, Coords, PlacedRingGameState),
    reduce_remaining_rings(PlacedRingGameState, Player, NewGameState).

% remove_ring(+GameState, +Player, +Coords, -NewGameState)
% Removes a  Ring from Player at Cords
remove_ring(GameState, Player, Coords, NewGameState) :-
    get_stack(GameState, Coords, Stack),
    owns_ring(Player, RingCode),
    % Fails in every case where the player''s ring is NOT on top of the Stack (eg: wrong player ring or a ball)
    append(PoppedStack, [RingCode], Stack),
    replace_stack(GameState, Coords, PoppedStack, NewGameState).

% move_ring(+GameState, +Player, +Displace, -NewGameState)
% Moves a ring from a cell to another
move_ring(GameState, Player, [FromCoords, ToCoords], NewGameState) :-
    % Cannot move to the same place
    is_pos_different(FromCoords, ToCoords),
    remove_ring(GameState, Player, FromCoords, RemovedGameState),
    place_ring(RemovedGameState, Player, ToCoords, NewGameState).



% ------------------------
%      BALL MOVEMENT
% ------------------------
% can_vault(+GameState, +Player, +Displace, -BallsToDisplace)~
% Checks if a ball can vault in a certain direction
can_vault(GameState, Player, [FromCoords, ToCoords], BallsToDisplace) :-
    % Already fails if not a valid trajectory in get_direction
    get_direction([FromCoords, ToCoords], Delta),
    add_coords(FromCoords, Delta, NextCoords),
    can_vault_aux(GameState, Player, [NextCoords, ToCoords], Delta, [], BallsToDisplace).

% can_vault_aux(+GameState, +Player, +Displace, +Delta, +BallsDisplaced, -BallsToDisplace)
% Auxiliary function of can_vault/4 that return the ballstodisplace after a vault if it is a possible one
% Can safely ignore the last stack (verification is done previously)
can_vault_aux(_, _, [Coords, Coords], _, BallsDisplaced, BallsDisplaced).
can_vault_aux(GameState, Player, [FromCoords, ToCoords], Delta, BallsDisplaced, BallsToDisplace) :-
    get_top_elem(GameState, FromCoords, TopElem),
    is_ball(TopElem),
    add_coords(FromCoords, Delta, NextCoords),
    ite(
        owns_ball(Player, TopElem),
        can_vault_aux(GameState, Player, [NextCoords, ToCoords], Delta, BallsDisplaced, BallsToDisplace),
        (
            append(BallsDisplaced, [FromCoords], NewBallsDisplaced),
            can_vault_aux(GameState, Player, [NextCoords, ToCoords], Delta, NewBallsDisplaced, BallsToDisplace)
        )
    ).

% can_move_ball(+GameState, +Player, +Displace, -BallsToDisplace)
% Checks if we can move a ball to a specified location
can_move_ball(GameState, Player, [FromCoords, ToCoords], BallsToDisplace) :-
    % Verify it is not the same position
    is_pos_different(FromCoords, ToCoords),
    % Verify if there is a player ball
    get_top_elem(GameState, FromCoords, FromLast),
    owns_ball(Player, FromLast),
    % Verify if there is a player ring to move to
    get_top_elem(GameState, ToCoords, ToLast),
    owns_ring(Player, ToLast),
    ite(
        is_adjacent(FromCoords, ToCoords),
        % If it is directly adjacent
        BallsToDisplace = [],
        % Can we vault and if so what balls need to be relocated
        can_vault(GameState, Player, [FromCoords, ToCoords], BallsToDisplace)
    ).

% remove_ball(+GameState, +Player, +Coords, -NewGameState)
% Removes a ball from Coords
remove_ball(GameState, Player, Coords, NewGameState) :-
    get_stack(GameState, Coords, Stack),
    owns_ball(Player, BallCode),
    append(PoppedStack, [BallCode], Stack),
    replace_stack(GameState, Coords, PoppedStack, NewGameState).

% place_ball(+GameState, +Player, +Coords, -NewGameState)~
% Places a new ball at Coords
place_ball(GameState, Player, Coords, NewGameState) :-
    get_stack(GameState, Coords, Stack),
    can_ball_be_placed(Stack, Player),
    owns_ball(Player, BallCode),
    append(Stack, [BallCode], NewStack),
    replace_stack(GameState, Coords, NewStack, NewGameState).

% displace_ball(+GameState, +Player, +Displace, -NewGameState)
% To be used for relocating enemy balls because it has less restrictions on movement
displace_ball(GameState, Player, [FromCoords, ToCoords], NewGameState) :-
    is_pos_different(FromCoords, ToCoords),
    remove_ball(GameState, Player, FromCoords, RemovedGameState),
    place_ball(RemovedGameState, Player, ToCoords, NewGameState).

% move_ball(+GameState, +Player, +Displace, -NewGameState, -BallsToDisplace)
% To be used when moving your own ball as we must consider vaulting
move_ball(GameState, Player, Displace, NewGameState, BallsToDisplace) :-
    can_move_ball(GameState, Player, Displace, BallsToDisplace),
    displace_ball(GameState, Player, Displace, NewGameState).

% move(+GameState, +Move, -NewGameState)
% Makes a move Move and return the new GameState
move(GameState, Move, NewGameState) :-
    %Split move into components
    new_move([RingFromCoords, RingToCoords], BallDisplace, BallRelocations, Player, Move),
    ite(
        is_negative_coords(RingFromCoords),
        move_ring(GameState, Player, [RingFromCoords, RingToCoords], RingPhaseGameState),
        place_new_ring(GameState, Player, RingToCoords, RingPhaseGameState)
    ),
    move_ball(RingPhaseGameState, Player, BallDisplace, MovedBallGameState, _),
    %Relocate balls after a vault
    next_player(Player, Enemy),
    relocate_balls(MovedBallGameState, Enemy, BallRelocations, NewGameState).

% relocate_balls(+GameState, +Player, +BallRelocations, -FinalGameState)
% Relocates each ball displaced by a vault
relocate_balls(GameState, _, [], GameState).
relocate_balls(GameState, Player, [Relocation | BallRelocations], FinalGameState) :-
    displace_ball(GameState, Player, Relocation, NextGameState),
    relocate_balls(NextGameState, Player, BallRelocations, FinalGameState).

% value(+GameState, +Player, -Value)
% Return a value for the board
% At the moment only takes into account the distance of the player's balls to the final cells
value(GameState, Player, Value) :-
    get_stacks_if(GameState, [is_ball_from_player_on_top_of_stack, Player], StackCoords),
    ball_distance_score(GameState, Player, StackCoords, Value).

% ball_distance_score(+GameState, +Player, +BallCoords, -Score)
% Returns the average of the distane of each ball to each final spot
ball_distance_score(GameState, Player, BallCoords, Score) :-
    get_initial_cells(GameState, Player, CoordList),
    nth0(0, BallCoords, Ball1),
    nth0(1, BallCoords, Ball2),
    nth0(2, BallCoords, Ball3),
    maplist(dist, [Ball1, Ball1, Ball1], CoordList, Dists1),
    maplist(dist, [Ball2, Ball2, Ball2], CoordList, Dists2),
    maplist(dist, [Ball3, Ball3, Ball3], CoordList, Dists3),
    avg(Dists1, Avg1),
    avg(Dists2, Avg2),
    avg(Dists3, Avg3),
    sumlist([Avg1, Avg2, Avg3], Score).


% STARTED AI STUFFS
% new_move(RingDisplace, BallDisplace, BallRelocations, Player, [RingDisplace, BallDisplace, BallRelocations, Player]).

lambda_can_ball_be_placed(Player, Stack) :-
    can_ball_be_placed(Stack, Player).
get_exposed_rings(GameState, Player, ExposedRings) :-
    get_stacks_if(GameState, [lambda_can_ball_be_placed, Player], ExposedRings).


lambda_can_ring_be_placed(Stack) :-
    can_ring_be_placed(Stack).
get_ring_placement_locations(GameState, PossibleRingPositions) :-
    get_stacks_if(GameState, [lambda_can_ring_be_placed], PossibleRingPositions).


lambda_coords_to_ring_displacement(Old, [[-1, -1], Old]).
valid_moves_add_ring_placement(GameState, Player, CurList, RingMoves) :-
    get_remaining_player_rings(GameState, Player, RemaingRings),
    it(RemaingRings =< 0, fail),
    get_ring_placement_locations(GameState, PossibleRingPositions),
    maplist(lambda_coords_to_ring_displacement, PossibleRingPositions, PossibleRingDisplacement),
    append(CurList, PossibleRingDisplacement, RingMoves).


valid_moves_add_ring_dislocation(GameState, Player, CurList, RingMoves) :-
    get_exposed_rings(GameState, Player, ExposedRings),
    get_ring_placement_locations(GameState, PosibleRingPositions),
    combinatorial_zip(ExposedRings, PosibleRingPositions, RingDisplacements),
    append(CurList, RingDisplacements, RingMoves).



% TODO: Valid moves is for current or next player?
valid_moves(GameState, ListOfMoves) :-
    Player = white,
    valid_moves_add_ring_placement(GameState, Player, [], PartialRingDisplacements),
    valid_moves_add_ring_dislocation(GameState, Player, PartialRingDisplacements, RingDisplacements),
    % RingDisplacements contains all possible ring phase moves, aka placing or moving a ring
    
    write(RingDisplacements),
    write('\n').


