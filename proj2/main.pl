:-use_module(library(lists)).
:-use_module(library(between)).

% init_state(-GameState)
% Returns the initial board that will be used to play the game
initial_state(
    game_state([
    [2, 2, 2, 2, 2, 2, 2, 2, 2],
    [2, 2, 2, 2, 2, 2, 2, 2, 2],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [1, 1, 1, 1, 1, 1, 1, 1, 1],
    [1, 1, 1, 1, 1, 1, 1, 1, 1]
], 1)).


% game_over(+GameState, -Winner)
game_over([
    [1, 1, 1, 1, 1, 1, 1, 1, 1],
    [1, 1, 1, 1, 1, 1, 1, 1, 1],
    [_, _, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _, _]
], 1).

game_over([
    [_, _, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _, _],
    [2, 2, 2, 2, 2, 2, 2, 2, 2],
    [2, 2, 2, 2, 2, 2, 2, 2, 2]
], 2).

% get_board(+GameState, -Board)
% Returns the Board of the GameState
get_board(game_state(Board, _), Board).

% get_player(+GameState, -Player)
% Returns the current Player of the GameState
get_player(game_state(_, Player), Player).

% decompose_position(+Position, -X, -Y)
% Return the X Y of the given position
decompose_position(position(X,Y), X, Y).

% get_position_X(+Position, -X)
% Returns the X of the given Position
get_position_X(position(X,_), X).

% get_position_Y(+Position, -Y)
% Returns the Y of the given Position
get_position_Y(position(_,Y), Y).

% decompose_play(+Move, -Piece, - Destination)
% Returns the relevant positions of the Move
decompose_play(play(Piece, Destination), Piece, Destination).

% get_board_position(+Board, +Position, -Value)
get_board_position(Board, Position, Value) :-
    get_position_X(Position, X),
    get_position_Y(Position, Y),
    length(Board, LenY),

    LenYMinus1 is LenY - 1, 
    between(0, LenYMinus1, Y), % >= 0 && < LenY
    nth0(Y, Board, Row),
    length(Row, LenX),

    LenXMinus1 is LenX - 1,
    between(0, LenXMinus1, X), % >= 0 && < LenX
    nth0(X, Row, Value).

% valid_position(+Board, +Position)
valid_position(Board, Position) :- 
    get_board_position(Board, Position, Value),
    Value =:= 0.

%neighbours(+Piece, -ListOfPositions)
% Returns a list with the position of the neighbours of the given Piece
neighbours(Piece, [P1,P2,P3,P4,P5,P6,P7,P8]) :-
    get_position_X(Piece, X),
    get_position_Y(Piece, Y),
    XP1 is X+1,
    XP2 is X+2,
    XM1 is X-1,
    XM2 is X-2,
    YP1 is Y+1,
    YP2 is Y+2,
    YM1 is Y-1,
    YM2 is Y-2,
    P1 = position(XP1,YP2),
    P2 = position(XP1,YM2),
    P3 = position(XM1,YP2),
    P4 = position(XM1,YM2),
    P5 = position(XP2,YP1),
    P6 = position(XP2,YM1),
    P7 = position(XM2,YP1),
    P8 = position(XM2,YM1).

% remove_invalid_positions(+Board, +Positions, -ValidPositions)
% Filters the given Positions list, returning a list with valid positions
% remove_invalid_positions(_, [], []).
% 
% remove_invalid_positions(Board, [H | Neighbours], [H | Aux]) :-
%     valid_position(Board, H),
%     remove_invalid_positions(Board, Neighbours, Aux).
% 
% remove_invalid_positions(Board, [_ | Neighbours], Aux) :-
%     remove_invalid_positions(Board, Neighbours, Aux).
    
% valid_positions_for_piece(+Board, +Piece, -ListOfPositions)
% Returns a list with valid position for the given Piece
% valid_positions_for_piece(Board, Piece, ListOfPositions) :-
%     neighbours(Piece, Neighbours),
%     member(PossibleMove, Neighbours),
%     remove_invalid_positions(Board, Neighbours, ListOfPositions).

valid_position_for_piece(Board, Piece, Move) :-
    neighbours(Piece, Neighbours),
    member(Move, Neighbours),
    valid_position(Board, Move).

isNotEmpty([_|_]).

% valid_play(+GameState, ?Play)
% Para cada Piece do current player:
%   obter plays dessa peça
%   concatenar ao resultado
valid_play(game_state(Board, Player), play(Position, Move)) :-
    get_board_position(Board, Position, Player),
    valid_position_for_piece(Board, Position, Move).

%validate_play(game_state(Board, Player), play(Piece, Destination)) :-

% switch_player(+OldPlayer, -NewPlayer).
switch_player(1, 2).
switch_player(2, 1).

change_value(Board, position(X, Y), NewValue, NewBoard) :-
    nth0(Y, Board, Row, R1),
    nth0(X, Row, _, R2),
    nth0(X, NewRow, NewValue, R2), % New row is equal to Row in all elements except index=PieceX (R2=R2)
    nth0(Y, NewBoard, NewRow, R1).

% move(+GameState, +Move, -NewGameState)
move(game_state(Board, Player), play(Piece, Destination), game_state(NewBoard, NewPlayer)) :-
    % validate play
    valid_play(game_state(Board, Player), play(Piece, Destination)),
    % update board (posiçaõ atual a 0, nova com nr do jogador)
    % update Player (NewPlayer IS Player + 1 % 1 -> não usar matematica aqui)
    change_value(Board, Piece, 0, B1),
    change_value(B1, Destination, Player, NewBoard),
    switch_player(Player, NewPlayer).

%position_with_value(Value, X, Y) :-
% goodbye world PLS ;-;
% goodbye world pls

%get_player_pieces_positions(Board, Player, ListOfPositions) :-

debug(NewState) :-
    initial_state(State),
    move(State, play(position(0,7),position(1,5)), NewState),
    write(NewState).