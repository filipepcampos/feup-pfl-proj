:- consult('interface.pl').
:- consult('logic.pl').

play_game :-
    repeat,
    display_menu(PlayerType1, PlayerType2),
    initial_state(GameState),
    display_game(GameState),
    game_cycle(GameState, PlayerType1, PlayerType2),
    fail.

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

%choose_move(GameState, computer-Level, Move) :-
%    valid_moves(GameState, Moves),
%    choose_move(Level, GameState, Moves, Move).
    
valid_moves(GameState, Moves) :-
    findall(Move, valid_moves(GameState, Move), Moves).

choose_move(1, _GameState, Moves, Move):-
    random_select(Move, Moves, _Rest).

% choose_move(2, GameState, Moves, Move):-
%     setof(Value-Mv, NewState^( member(Mv, Moves),
%     move(GameState, Mv, NewState),
%     evaluate_board(NewState, Value) ), [_V-Move|_]).
% % evaluate_board assumes lower value is better