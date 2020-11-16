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
place_ring(GameState, Player, RowIndex, ColIndex, NewGameState) :-
    get_stack(GameState, RowIndex, ColIndex, Stack),
    can_ring_be_placed(Stack),
    owns_ring(Player, RingCode),
    append(Stack, [RingCode], NewStack),
    replace_stack(GameState, RowIndex, ColIndex, NewStack, NewGameState).

place_new_ring(GameState, Player, RowIndex, ColIndex, NewGameState) :-
    % Does the player still have rings to place?
    get_remaining_player_rings(GameState, Player, Rings),
    Rings > 0,
    place_ring(GameState, Player, RowIndex, ColIndex, PlacedRingGameState),
    reduce_remaining_rings(PlacedRingGameState, Player, NewGameState).

remove_ring(GameState, Player, RowIndex, ColIndex, NewGameState) :-
    get_stack(GameState, RowIndex, ColIndex, Stack),
    owns_ring(Player, RingCode),
    % Fails in every case where the player''s ring is NOT on top of the Stack (eg: wrong player ring or a ball)
    append(PoppedStack, [RingCode], Stack),
    replace_stack(GameState, RowIndex, ColIndex, PoppedStack, NewGameState).

move_ring(GameState, Player, FromRowIndex, FromColIndex, ToRowIndex, ToColIndex, NewGameState) :-
    % Cannot move to the same place
    is_pos_different(FromRowIndex, FromColIndex, ToRowIndex, ToColIndex),
    remove_ring(GameState, Player, FromRowIndex, FromColIndex, RemovedGameState),
    place_ring(RemovedGameState, Player, ToRowIndex, ToColIndex, NewGameState).



% ------------------------
%      BALL MOVEMENT
% ------------------------
can_vault(GameState, Player, FromRowIndex, FromColIndex, ToRowIndex, ToColIndex, BallsToDisplace) :-
    % Already fails if not a valid trajectory in get_direction
    get_direction(FromRowIndex, FromColIndex, ToRowIndex, ToColIndex, DeltaRow, DeltaCol),
    NextRow is FromRowIndex + DeltaRow,
    NextCol is FromColIndex + DeltaCol,
    can_vault_aux(GameState, Player, NextRow, NextCol, ToRowIndex, ToColIndex, DeltaRow, DeltaCol, [], BallsToDisplace).

% Can safely ignore the last stack (verification is done previously)
can_vault_aux(_, _, RowIndex, ColIndex, RowIndex, ColIndex, _, _, BallsDisplaced, BallsDisplaced).
can_vault_aux(GameState, Player, RowIndex, ColIndex, ToRowIndex, ToColIndex, DeltaRow, DeltaCol, BallsDisplaced, BallsToDisplace) :-
    get_top_elem(GameState, RowIndex, ColIndex, TopElem),
    is_ball(TopElem),
    NextRow is RowIndex + DeltaRow,
    NextCol is ColIndex + DeltaCol,
    ite(
        owns_ball(Player, TopElem),
        can_vault_aux(GameState, Player, NextRow, NextCol, ToRowIndex, ToColIndex, DeltaRow, DeltaCol, BallsDisplaced, BallsToDisplace),
        (
            append(BallsDisplaced, [[RowIndex, ColIndex]], NewBallsDisplaced),
            can_vault_aux(GameState, Player, NextRow, NextCol, ToRowIndex, ToColIndex, DeltaRow, DeltaCol, NewBallsDisplaced, BallsToDisplace)
        )
    ).


can_move_ball(GameState, Player, FromRowIndex, FromColIndex, ToRowIndex, ToColIndex, BallsToDisplace) :-
    % Verify it is not the same position
    is_pos_different(FromRowIndex, FromColIndex, ToRowIndex, ToColIndex),
    % Verify if there is a player ball
    get_top_elem(GameState, FromRowIndex, FromColIndex, FromLast),
    owns_ball(Player, FromLast),
    % Verify if there is a player ring to move to
    get_top_elem(GameState, ToRowIndex, ToColIndex, ToLast),
    owns_ring(Player, ToLast),
    ite(
        is_adjacent(FromRowIndex, FromColIndex, ToRowIndex, ToColIndex),
        % If it is directly adjacent
        BallsToDisplace = [],
        % Can we vault and if so what balls need to be relocated
        can_vault(GameState, Player, FromRowIndex, FromColIndex, ToRowIndex, ToColIndex, BallsToDisplace)
    ).

remove_ball(GameState, Player, RowIndex, ColIndex, NewGameState) :-
    get_stack(GameState, RowIndex, ColIndex, Stack),
    owns_ball(Player, BallCode),
    append(PoppedStack, [BallCode], Stack),
    replace_stack(GameState, RowIndex, ColIndex, PoppedStack, NewGameState).

place_ball(GameState, Player, RowIndex, ColIndex, NewGameState) :-
    get_stack(GameState, RowIndex, ColIndex, Stack),
    can_ball_be_placed(Stack, Player),
    owns_ball(Player, BallCode),
    append(Stack, [BallCode], NewStack),
    replace_stack(GameState, RowIndex, ColIndex, NewStack, NewGameState).

% % To be used for relocating enemy balls because it has less restrictions on movement
displace_ball(GameState, Player, FromRowIndex, FromColIndex, ToRowIndex, ToColIndex, NewGameState) :-
    is_pos_different(FromRowIndex, FromColIndex, ToRowIndex, ToColIndex),
    remove_ball(GameState, Player, FromRowIndex, FromColIndex, RemovedGameState),
    place_ball(RemovedGameState, Player, ToRowIndex, ToColIndex, NewGameState).

% % To be used when moving your own ball as we must consider vaulting
move_ball(GameState, Player, FromRowIndex, FromColIndex, ToRowIndex, ToColIndex, NewGameState, BallsToDisplace) :-
    can_move_ball(GameState, Player, FromRowIndex, FromColIndex, ToRowIndex, ToColIndex, BallsToDisplace),
    displace_ball(GameState, Player, FromRowIndex, FromColIndex, ToRowIndex, ToColIndex, NewGameState).



move(GameState, Move, NewGameState) :-
    new_move(RingDisplace, BallDisplace, BallRelocations, Player, Move),
    RingDisplace = [RingFromRow, RingFromCol, RingToRow, RingToCol],
    ite(
        (RingFromRow =\= -1, RingFromCol =\= -1),
        move_ring(GameState, Player, RingFromRow, RingFromCol, RingToRow, RingToCol, RingPhaseGameState),
        place_new_ring(GameState, Player, RingToRow, RingToCol, RingPhaseGameState)
    ),
    BallDisplace = [BallFromRow, BallFromCol, BallToRow, BallToCol],
    move_ball(RingPhaseGameState, Player, BallFromRow, BallFromCol, BallToRow, BallToCol, NewGameState, _).
