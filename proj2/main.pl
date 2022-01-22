:-use_module(library(random)).
:- consult('interface.pl').
:- consult('logic.pl').

play :-
    repeat,
    not(play_game).

play_game :-
    display_menu(PlayerType1, PlayerType2),
    initial_state(GameState),
    display_game(GameState),
    game_cycle(GameState, PlayerType1, PlayerType2).

game_cycle(GameState, _, _):-
    game_over(GameState, Winner), !,
    congratulate(Winner).

game_cycle(game_state(Board, Player), PlayerType1, PlayerType2):-
    current_player_type(Player, PlayerType1, PlayerType2, CurrentPlayerType),
    choose_move(game_state(Board, Player), CurrentPlayerType, Move),
    move(game_state(Board, Player), Move, NewGameState),
    display_game(NewGameState),
    !,
    game_cycle(NewGameState, PlayerType1, PlayerType2).

% current_player_type(+Player, +PlayerType1, +PlayerType2, -CurrentPlayerType)
current_player_type(1, CurrentPlayerType, _, CurrentPlayerType).
current_player_type(2, _, CurrentPlayerType, CurrentPlayerType).

% choose_move(+GameState, +PlayerType, -Move)
choose_move(GameState, human, Move):-
    repeat,
    get_move(Move),
    valid_move(GameState, Move).

% choose_move(+GameState, +computer-AILevel, -Move)
choose_move(GameState, computer-3, Move) :-
    valid_moves(GameState, Moves),
    choose_move(2, GameState, Moves, Move), % Level 3 uses the same algorithm as level 2 but with a more complex moves list
    write_move(Move).

choose_move(GameState, computer-Level, Move) :-
    valid_moves_without_jump(GameState, Moves),
    choose_move(Level, GameState, Moves, Move),
    write_move(Move).
    
% valid_moves(+GameState, -Moves)
valid_moves(GameState, Moves) :-
    findall(Move, valid_move(GameState, Move), Moves).

% valid_moves_without_jump(+GameState, -Moves)
valid_moves_without_jump(GameState, Moves) :-
    findall(Move, valid_move_without_jump(GameState, Move), Moves).

% choose_move(+AILevel, +GameState, +Moves, -Move)
choose_move(1, _GameState, Moves, Move):-
    random_select(Move, Moves, _Rest).

choose_move(2, GameState, Moves, Move):-
    setof(Value-Mv, NewState^( member(Mv, Moves),
                                move(GameState, Mv, NewState),
                                value(NewState, Value) 
                            ), Results),
    keyclumps(Results, Clumps), % Group up all possible moves by their key (Value)
    head(Clumps, BestPossibleMoves),  % Get all the moves with minimum key (Minimum Value)
    random_select(BestValue-Move, BestPossibleMoves, _Rest). % From all the best moves (that yield the same value), choose a random one.