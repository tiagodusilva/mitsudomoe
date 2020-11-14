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
    remove_ring(GameState, Player, FromRowIndex, FromColIndex, RemovedGameState),
    place_ring(RemovedGameState, Player, ToRowIndex, ToColIndex, NewGameState).



% ------------------------
%      BALL MOVEMENT
% ------------------------
is_adjacent(FromRowIndex, FromColIndex, ToRowIndex, ToColIndex) :-
    RowAdj is abs(FromRowIndex - ToRowIndex),
    ColAdj is abs(FromColIndex - ToColIndex),
    % Same cell
    (RowAdj + ColAdj) =\= 0,
    RowAdj =< 1,
    ColAdj =< 1.

% Given a value, set it to -1, 0 or 1
normalize_delta(Value, Delta) :-
    ite(
        Value =:= 0,
        Delta is 0,
        (
            ite(
                Value < 0,
                Delta is -1,
                Delta is 1
            )
        )
    ).

get_direction(FromRowIndex, FromColIndex, ToRowIndex, ToColIndex, DeltaRow, DeltaCol) :-
    RowAdj is (ToRowIndex - FromRowIndex),
    ColAdj is (ToColIndex - FromColIndex),
    % Verify if perfect diagonal, horizontal or vertical (in order)
    \+ (
        \+ (
            RowAdj =\= 0,
            ColAdj =\= 0,
            % Only true if perfect diagonal
            abs(RowAdj) =:= abs(ColAdj)
        ),
        \+ (
            RowAdj =\= 0,
            ColAdj =:= 0
        ),
        \+ (
            RowAdj =:= 0,
            ColAdj =\= 0
        )
    ),
    normalize_delta(RowAdj, DeltaRow),
    normalize_delta(ColAdj, DeltaCol).


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


% % To be used for relocating enemy balls because it has no restrictions on movement
% displace_ball(GameState, Player, FromRowIndex, FromColIndex, ToRowIndex, ToColIndex, NewGameState) :-

% % To be used when moving your own ball
% move_ball(GameState, Player, FromRowIndex, FromColIndex, ToRowIndex, ToColIndex, NewGameState, BallsToDisplace) :-
