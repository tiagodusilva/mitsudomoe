valid_moves(GameState, Player, ListOfMoves) :-
    fail.    

reduce_remaining_rings(GameState, Player, NewGameState) :-
    get_remaining_player_rings(GameState, Player, Rings),
    Rings > 0,
    NewRings is Rings - 1,
    replace_remaining_rings(GameState, Player, NewRings, NewGameState).

can_ring_be_placed([]).
can_ring_be_placed(Stack) :-
    last(Stack, LastElem),
    is_ring(LastElem).

place_ring(GameState, Player, RowIndex, ColIndex, NewGameState) :-
    get_stack(GameState, RowIndex, ColIndex, Stack),
    !,
    can_ring_be_placed(Stack),
    owns_ring(Player, RingCode),
    append(Stack, [RingCode], NewStack),
    replace_stack(GameState, RowIndex, ColIndex, NewStack, NewGameState).

place_new_ring(GameState, Player, RowIndex, ColIndex, NewGameState) :-
    % Does the player still have rings to place?
    get_remaining_player_rings(GameState, Player, Rings),
    Rings > 0,
    !,
    place_ring(GameState, Player, RowIndex, ColIndex, PlacedRingGameState),
    reduce_remaining_rings(PlacedRingGameState, Player, NewGameState).
    
