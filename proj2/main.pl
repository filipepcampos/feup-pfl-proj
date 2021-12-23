:-use_module(library(lists)).

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

% get_board(+GameState, -Board)
% Returns the Board of the GameState
get_board(game_state(Board, _), Board).

% get_player(+GameState, -Player)
% Returns the current Player of the GameState
get_player(game_state(_, Player), Player).

% get_position_X(+Position, -X)
% Returns the X of the given Position
get_position_X(position(X,_), X).

% get_position_Y(+Position, -Y)
% Returns the Y of the given Position
get_position_Y(position(_,Y), Y).

% decompose_move(+Move, -Piece, - Destination)
% Returns the relevant positions of the Move
decompose_move(move(Piece, Destination), Piece, Destination).

% get_board_position(+Board, +Position, -Value)
get_board_position(Board, Position, Value) :-
    get_position_X(Position, X),
    get_position_Y(Position, Y),
    length(Board, LenY),
    Y < LenY,
    Y >= 0,
    nth0(Y, Board, Row),
    length(Row, LenX),
    X < LenX,
    X >= 0,
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
remove_invalid_positions(_, [], []).

remove_invalid_positions(Board, [H | Neighbours], [H | Aux]) :-
    valid_position(Board, H),
    remove_invalid_positions(Board, Neighbours, Aux).

remove_invalid_positions(Board, [_ | Neighbours], Aux) :-
    remove_invalid_positions(Board, Neighbours, Aux).
    
% valid_positions_for_piece(+Board, +Piece, -ListOfPositions)
% Returns a list with valid position for the given Piece
valid_positions_for_piece(Board, Piece, ListOfPositions) :-
    neighbours(Piece, Neighbours),
    remove_invalid_positions(Board, Neighbours, ListOfPositions).

% valid_moves(+GameState, -ListOfMoves)
% Para cada Piece do current player:
%   obter moves dessa peça
%   concatenar ao resultado
%valid_moves(game_state(Board, Player), ListOfMoves) :-
    %get_board_position(Board, Position, Player),


% move(+GameState, +Move, -NewGameState)
% game_over(+GameState, -Winner)

position_with_value(Value, X, Y) :-
% goodbye world PLS ;-;
% goodbye world pls

get_player_pieces_positions(Board, Player, ListOfPositions) :-


debug(X) :-
    get_board_position([
[2, 2, 2, 2, 2, 2, 2, 2, 2],
[2, 2, 2, 2, 2, 2, 2, 2, 2],
[0, 0, 0, 0, 0, 0, 0, 0, 0],
[0, 0, 0, 0, 0, 0, 0, 0, 0],
[0, 0, 0, 0, 0, 0, 0, 0, 0],
[0, 0, 0, 0, 0, 0, 0, 0, 0],
[0, 0, 0, 0, 0, 0, 0, 0, 0],
[1, 1, 1, 1, 1, 1, 1, 1, 1],
[1, 1, 1, 1, 1, 1, 1, 1, 1]
], X, 2).