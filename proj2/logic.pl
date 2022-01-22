:-use_module(library(lists)).
:-use_module(library(between)).

%initial_state(-GameState)
%Returns the initial board that will be used to play the game
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

% Used exclusively to test end-game conditions, not a real initial state
% initial_state(
%     game_state([
%     [1, 1, 1, 0, 1, 1, 1, 1, 1],
%     [1, 1, 1, 1, 1, 1, 1, 1, 1],
%     [0, 0, 0, 0, 1, 0, 0, 0, 0],
%     [0, 0, 0, 0, 0, 0, 0, 0, 0],
%     [0, 0, 0, 0, 0, 0, 0, 0, 0],
%     [0, 0, 0, 0, 0, 0, 2, 0, 0],
%     [0, 0, 0, 0, 0, 0, 0, 0, 0],
%     [2, 2, 2, 2, 2, 2, 2, 0, 2],
%     [2, 2, 2, 2, 2, 2, 2, 2, 2]
% ], 1)).


% game_over(+GameState, -Winner)
game_over(game_state([
    [1, 1, 1, 1, 1, 1, 1, 1, 1],
    [1, 1, 1, 1, 1, 1, 1, 1, 1],
    [_, _, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _, _]
],_), 1).

game_over(game_state([
    [_, _, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _, _],
    [2, 2, 2, 2, 2, 2, 2, 2, 2],
    [2, 2, 2, 2, 2, 2, 2, 2, 2]
],_), 2).


% decompose_position(+Position, -X, -Y)
% Return the X Y of the given position
decompose_position(position(X,Y), X, Y).

% get_board_position(+Board, +Position, -Value)
% Get value of the given position in the board
get_board_position(Board, Position, Value) :-
    decompose_position(Position, X, Y),
    length(Board, LenY),

    LenYMinus1 is LenY - 1, 
    between(0, LenYMinus1, Y), % >= 0 && < LenY
    nth0(Y, Board, Row),
    length(Row, LenX),

    LenXMinus1 is LenX - 1,
    between(0, LenXMinus1, X), % >= 0 && < LenX
    nth0(X, Row, Value).

% valid_position(+Board, +Position)
% Check if position is an valid destination (i.e it has value 0)
valid_position(Board, Position) :- 
    get_board_position(Board, Position, 0).

% neighbours(+Piece, -ListOfPositions)
% Returns a list with the position of the neighbours of the given Piece
neighbours(Piece, [P1,P2,P3,P4,P5,P6,P7,P8]) :-
    decompose_position(Piece, X, Y),
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

% not(+X)
not(X):- X, !, fail.
not(_X).

% valid_position_for_piece(+GameState, +Piece, +Destination)
% Check if the given Piece can be moved to the Destination position
valid_position_for_piece(game_state(Board, Player), Piece, Destination) :-
    valid_position_for_piece_without_jump(game_state(Board, Player), Piece, Destination).

valid_position_for_piece(game_state(Board, Player), Piece, Destination) :-
    valid_position(Board, Destination),
    valid_position_with_jump(game_state(Board, Player), Piece, Destination, []).

% valid_position_for_piece_without_jump(+GameState, +Piece, +Destination)
% Check if the given Piece can be moved to the Destination position, without using the mechanic of jumping on another piece
valid_position_for_piece_without_jump(game_state(Board, _), Piece, Destination) :-
    valid_position(Board, Destination),
    neighbours(Piece, Neighbours),
    member(Destination, Neighbours).


% valid_position_with_jump(+GameState, +Piece, +Destination, +Visited)
%  Check if the given Piece can be moved to the Destination position by jumping on intermediate pieces.

% Terminate the jumping sequence by moving onto an empty Destination
valid_position_with_jump(game_state(_, _), Piece, Destination, _) :-
    neighbours(Piece, Neighbours),
    member(Destination, Neighbours). % Check if destination is a neighbour of the last piece

% Check if Destination can be reached by jumping on an intermediate piece that's neighboring the current piece
valid_position_with_jump(game_state(Board, Player), Piece, Destination, Visited) :-
    neighbours(Piece, Neighbours),
    member(IntermediatePiece, Neighbours), % For each piece neighboring the current Piece
    switch_player(Player, Opponent),
    get_board_position(Board, IntermediatePiece, Opponent), % IntermediatePiece is an oppponent's piece
    not(member(IntermediatePiece, Visited)), % Avoid infinite recursion
    valid_position_with_jump(game_state(Board, Player), IntermediatePiece, Destination, [IntermediatePiece|Visited]).


% valid_move(+GameState, ?Move)
valid_move(game_state(Board, Player), move(Piece, Destination)) :-
    get_board_position(Board, Piece, Player),
    valid_position_for_piece(game_state(Board, Player), Piece, Destination).

% valid_move_without_jump(+GameState, ?Move)
valid_move_without_jump(game_state(Board, Player), move(Piece, Destination)) :-
    get_board_position(Board, Piece, Player),
    valid_position_for_piece_without_jump(game_state(Board, Player), Piece, Destination).

% switch_player(+OldPlayer, -NewPlayer).
switch_player(1, 2).
switch_player(2, 1).

% change_value(+Board, +Position, +NewValue, -NewBoard)
% Change the value of a board position, returning a new board
change_value(Board, position(X, Y), NewValue, NewBoard) :-
    nth0(Y, Board, Row, R1),
    nth0(X, Row, _, R2),
    nth0(X, NewRow, NewValue, R2), % New row is equal to Row in all elements except index=PieceX (R2=R2)
    nth0(Y, NewBoard, NewRow, R1).

% move(+GameState, +Move, -NewGameState)
move(game_state(Board, Player), move(Piece, Destination), game_state(NewBoard, NewPlayer)) :-
    change_value(Board, Piece, 0, B1), % 'Pick up' the piece
    change_value(B1, Destination, Player, NewBoard), % 'Place it' on the destination
    switch_player(Player, NewPlayer).


% value(+GameState, -Value)
% Evaluate board
value(game_state(Board, Opponent), Value) :-
    switch_player(Opponent, Player), % Move auto-switches the player, but we want the value from the perspective of the player who just moved.
    setof(Piece, get_board_position(Board, Piece, Player), Pieces), % All player's pieces
    calculate_value(Pieces, Player, 0, Value).


% piece_value(+Piece, +Player, -PieceValue)
% Each piece's value is calculated by the distance to the opposite side,
% the closer it is the lesser the value is. (AI will try to minimize the value)
piece_value(Piece, 1, PieceValue) :-
    decompose_position(Piece, _, Row),
    PieceValue is Row.

piece_value(Piece, 2, PieceValue) :-
    decompose_position(Piece, _, Row),
    PieceValue is 8 - Row.

% calculate_value(+PieceValues, +Player, +CurrentValue, -OutputValue)
% Sum up all individual piece's value to obtain the GameState value
calculate_value([], _, OutputValue, OutputValue).

calculate_value([H | T], Player, CurrentValue, OutputValue) :-
    piece_value(H, Player, PieceValue),
    NewValue is CurrentValue + PieceValue,
    calculate_value(T, Player, NewValue, OutputValue).