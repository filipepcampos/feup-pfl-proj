:-use_module(library(between)).

display_menu(PlayerType1, PlayerType2) :-
    repeat,
    display_title,
    display_authors,
    display_options,
    get_option(Option),
    !,
    parse_option(Option, PlayerType1, PlayerType2).

display_title :-
    write(' _ __ ___ _ __  _ __   __ _  __ _ _ __ __| | ___ _ __\n'),
    write('| \'__/ _ \\ \'_ \\| \'_ \\ / _` |/ _` | \'__/ _` |/ _ \\ \'_ \\ \n'),
    write('| | |  __/ | | | |_) | (_| | (_| | | | (_| |  __/ | | |\n'),
    write('|_|  \\___|_| |_| .__/ \\__,_|\\__,_|_|  \\__,_|\\___|_| |_|\n'),
    write('               | |                                     \n'),
    write('               |_|                                     \n\n').

display_authors :-
    write('                     Filipe Campos\n'),
    write('                  Francisco Cerqueira\n'),
    write('       -------------------------------------------\n\n').

display_options :-
    write('                 1. Player vs Player\n'),
    write('                 2. Player vs Computer\n'),
    write('                 3. Computer vs Player\n'),
    write('                 4. Computer vs Computer\n\n'),
    write('                 0. Exit\n').

get_option(Option) :-
    write('Write you option:\n > '),
    get_code(CharCode),
    CharCode \= 10,
    peek_code(N),  % assure \n
    skip_line,
    !,
    N == 10,
    Option is CharCode - 48,
    between(0, 4, Option).

% parse_option(+Option, -PlayerType1, -PlayerType2)
parse_option(1, human, human).
parse_option(2, human, computer-2).
parse_option(3, computer-2, human).
parse_option(4, computer-2, computer-2).

get_move(move(position(PieceColumn, PieceRow), position(DestinationColumn, DestinationRow))) :-
    write('Choose Move:\n'),
    write('  Piece:\n'),
    get_position(PieceColumn-PieceRow),
    write('  Destination:\n'),
    get_position(DestinationColumn-DestinationRow).

get_position(Column-Row) :- 
    write('  > '),
    get_code(ColumnCode),
    ColumnCode \= 10,
    Column is ColumnCode - 97,
    between(0, 8, Column),
    get_code(RowCode),
    RowCode \= 10,
    skip_line,
    Row is RowCode - 49, % -48 - 1
    between(0, 8, Row).

write_move(move(position(PieceColumn, PieceRow), position(DestinationColumn, DestinationRow))) :-
    write_position(PieceColumn-PieceRow),
    write(' to '),
    write_position(DestinationColumn-DestinationRow),
    write('\n\n').

write_position(Column-Row) :-
    ColumnCode is Column + 97,
    RowCode is Row + 49,
    put_code(ColumnCode),
    put_code(RowCode).

display_game(game_state(Board, Player)) :-
    display_header,
    display_board(Board, 1),
    display_player(Player).

display_header :-
    write('    a b c d e f g h i\n'),
    write('  -------------------\n').

display_row([]).
display_row([H | T]) :-
    write(H),
    write(' '),
    display_row(T).
    
display_board([], _).
display_board([ Row | RestOfBoard], RowNumber) :-
    write(RowNumber),
    write(' | '),
    display_row(Row),
    write('\n'),
    NewRowNumber is RowNumber + 1,
    display_board(RestOfBoard, NewRowNumber).

display_player(Player) :-
    write('\nPlayer '),
    write(Player),
    write('\'s turn.\n').

% congratulate(+Winner)
congratulate(Winner) :-
    write('\n\nCongratulations!\nPlayer '),
    write(Winner),
    write(' wins!\n').