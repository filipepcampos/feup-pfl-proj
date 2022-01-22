:- consult('interface.pl').

play_game:-
    initial_state(GameState-Player),
    display_game(GameState-Player),
    game_cycle(GameState-Player).

game_cycle(GameState-Player):-
    game_over(GameState, Winner), !,
    congratulate(Winner).

game_cycle(GameState-Player):-
    choose_move(GameState, Player, Move),
    move(GameState, Move, NewGameState),
    next_player(Player, NextPlayer),
    display_game(GameState-NextPlayer), !,
    game_cycle(NewGameState-NextPlayer).


choose_move(GameState, human, Move):-
% interaction to select move

choose_move(GameState, computer-Level, Move):-
    valid_moves(GameState, Moves),
    choose_move(Level, GameState, Moves, Move).
valid_moves(GameState, Moves):-
    findall(Move, move(GameState, Move, NewState), Moves).
choose_move(1, _GameState, Moves, Move):-
    random_select(Move, Moves, _Rest).
choose_move(2, GameState, Moves, Move):-
    setof(Value-Mv, NewState^( member(Mv, Moves),
    move(GameState, Mv, NewState),
    evaluate_board(NewState, Value) ), [_V-Move|_]).
% evaluate_board assumes lower value is better