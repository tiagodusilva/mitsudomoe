:- use_module(library(lists)).

% valid_moves(GameState, Player, ListOfMoves) :-
%     fail.    

reduce_remaining_rings(GameState, Player, NewGameState) :-
    get_remaining_player_rings(GameState, Player, Rings),
    Rings > 0,
    NewRings is Rings - 1,
    replace_remaining_rings(GameState, Player, NewRings, NewGameState).

can_ring_be_placed([]).
can_ring_be_placed(Stack) :-
    last(Stack, LastElem),
    is_ring(LastElem).

can_ball_be_placed(Stack, Player) :-
    last(Stack, LastElem),
    owns_ring(Player, LastElem).

% ------------------------
%      RING MOVEMENT
% ------------------------
place_ring(GameState, Player, Coords, NewGameState) :-
    get_stack(GameState, Coords, Stack),
    can_ring_be_placed(Stack),
    owns_ring(Player, RingCode),
    append(Stack, [RingCode], NewStack),
    replace_stack(GameState, Coords, NewStack, NewGameState).

place_new_ring(GameState, Player, Coords, NewGameState) :-
    % Does the player still have rings to place?
    get_remaining_player_rings(GameState, Player, Rings),
    Rings > 0,
    place_ring(GameState, Player, Coords, PlacedRingGameState),
    reduce_remaining_rings(PlacedRingGameState, Player, NewGameState).

remove_ring(GameState, Player, Coords, NewGameState) :-
    get_stack(GameState, Coords, Stack),
    owns_ring(Player, RingCode),
    % Fails in every case where the player''s ring is NOT on top of the Stack (eg: wrong player ring or a ball)
    append(PoppedStack, [RingCode], Stack),
    replace_stack(GameState, Coords, PoppedStack, NewGameState).

move_ring(GameState, Player, [FromCoords, ToCoords], NewGameState) :-
    % Cannot move to the same place
    is_pos_different(FromCoords, ToCoords),
    remove_ring(GameState, Player, FromCoords, RemovedGameState),
    place_ring(RemovedGameState, Player, ToCoords, NewGameState).



% ------------------------
%      BALL MOVEMENT
% ------------------------
can_vault(GameState, Player, [FromCoords, ToCoords], BallsToDisplace) :-
    % Already fails if not a valid trajectory in get_direction
    get_direction([FromCoords, ToCoords], Delta),
    add_coords(FromCoords, Delta, NextCoords),
    can_vault_aux(GameState, Player, [NextCoords, ToCoords], Delta, [], BallsToDisplace).

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

remove_ball(GameState, Player, Coords, NewGameState) :-
    get_stack(GameState, Coords, Stack),
    owns_ball(Player, BallCode),
    append(PoppedStack, [BallCode], Stack),
    replace_stack(GameState, Coords, PoppedStack, NewGameState).

place_ball(GameState, Player, Coords, NewGameState) :-
    get_stack(GameState, Coords, Stack),
    can_ball_be_placed(Stack, Player),
    owns_ball(Player, BallCode),
    append(Stack, [BallCode], NewStack),
    replace_stack(GameState, Coords, NewStack, NewGameState).

% % To be used for relocating enemy balls because it has less restrictions on movement
displace_ball(GameState, Player, [FromCoords, ToCoords], NewGameState) :-
    is_pos_different(FromCoords, ToCoords),
    remove_ball(GameState, Player, FromCoords, RemovedGameState),
    place_ball(RemovedGameState, Player, ToCoords, NewGameState).

% % To be used when moving your own ball as we must consider vaulting
move_ball(GameState, Player, Displace, NewGameState, BallsToDisplace) :-
    can_move_ball(GameState, Player, Displace, BallsToDisplace),
    displace_ball(GameState, Player, Displace, NewGameState).


move(GameState, Move, NewGameState) :-
    new_move([RingFromCoords, RingToCoords], BallDisplace, BallRelocations, Player, Move),
    ite(
        is_negative_coords(RingFromCoords),
        move_ring(GameState, Player, [RingFromCoords, RingToCoords], RingPhaseGameState),
        place_new_ring(GameState, Player, RingToCoords, RingPhaseGameState)
    ),
    move_ball(RingPhaseGameState, Player, BallDisplace, MovedBallGameState, _),
    next_player(Player, Enemy),
    relocate_balls(MovedBallGameState, Enemy, BallRelocations, NewGameState).


relocate_balls(GameState, _, [], GameState).
relocate_balls(GameState, Player, [Relocation | BallRelocations], FinalGameState) :-
    displace_ball(GameState, Player, Relocation, NextGameState),
    relocate_balls(NextGameState, Player, BallRelocations, FinalGameState).

