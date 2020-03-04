-module(tictactoe).

-behaviour(gen_server).

-include_lib("stdlib/include/qlc.hrl").

%% API
-export([board_move/3, check_rows/1,
	 current_player/1, next_turn/1]).

-export([get_board/0, is_finished/0, move/2,
	 print_board/0, print_board/1, restart/0, restart/1,
	 show_board/1, start_link/0, start_link/1]).

%% Genserver callbacks
-export([code_change/3, handle_call/3, handle_cast/2,
	 init/1, terminate/2]).

-export([in_list/2]).

-define(MAX_MOVES, 9).

-define(MIN_MOVES, 0).

-define(PLAYERS, [1, 2]).

%%%====================================================================
%%% API
%%%====================================================================

% Starts with an empty board.
start_link() -> start_link([]).

% Starts with a preconfigured board.
start_link(Board) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Board,
			  []).

init(Board) -> {ok, Board}.

restart() -> gen_server:call(?MODULE, restart).

% restart(Board) ->
%     gen_server:call(?MODULE, {restart, Board}).

restart(_Board) ->
    gen_server:call(?MODULE,
		    {restart,
		     [{0, 0, 2}, {0, 1, 1}, {0, 2, 2}, {1, 0, 2}, {1, 1, 1},
		      {1, 2, 1}, {2, 0, 1}, {2, 2, 2}, {2, 1, 2}]}).

move(X, Y) -> 
    Board = get_board(),
    board_move(X, Y, Board).

get_board() -> gen_server:call(?MODULE, get_board).

% show_board(Board) ->
%     % Return board
%     ok.

%%%====================================================================
%%% genserver
%%%====================================================================

handle_call({test, Value}, _From, Board) ->
    NewState = [Value | Board], {reply, NewState, NewState};
% Set the new state to an empty board.
handle_call(get_board, _From, Board) ->
    {reply, Board, Board};
handle_call(restart, _From, _Board) -> {reply, [], []};
% Set the new board as current state.
handle_call({restart, NewBoard}, _From, _Board) ->
    {reply, NewBoard, NewBoard}.

handle_cast(restart, _State) -> {noreply, []}.

terminate(normal, _) -> ok.

code_change(_Old, State, _Extra) -> {ok, State}.

%%%====================================================================
%%% internal functions
%%%====================================================================

% % If 9 moves are made, no more possibilities remain.
is_finished() ->
    Board = get_board(),
    Length = length(Board),
    % cols, rows, diags, full
    check_rows(lists:keysort(1, Board)) or
      check_rows(lists:keysort(2, Board))
      or in_list([1, 2], Board)
      or
      (Length > (?MIN_MOVES)) and (Length < (?MAX_MOVES) + 1)
	and (Length / (?MAX_MOVES) == 1).

check_rows([]) -> false; % no combinations of 3 found.
check_rows([{X1, Y1, P1}, {X2, Y2, P2}, {X3, Y3, P3}
	    | Tail]) ->
    if (P1 == P2) and (P2 == P3) and
	 ((X1 == X2) and (X2 == X3) or
	    (Y1 == Y2) and (Y2 == Y3)) ->
	   true;
       true ->
	   if length(Tail) > 3 -> check_rows(Tail);
	      true -> false
	   end
    end.

in_list([], _Board) -> false;
in_list([P | Players], Board) ->
    V1 = lists:member({0, 0, P}, Board) and
	   lists:member({1, 1, P}, Board)
	   and lists:member({2, 2, P}, Board),
    V2 = lists:member({0, 2, P}, Board) and
	   lists:member({1, 1, P}, Board)
	   and lists:member({2, 0, P}, Board),
    if V1 or V2 == true -> true;
       true -> in_list(Players, Board)
    end.

% This is just a helper for in the REPL.
print_board() -> print_board(get_board()).

% This allows you to test printing without the server working.
print_board(Board) -> io:fwrite(show_board(Board)).

show_board(Board) ->
    print_row(Board, [{0, 0}, {0, 1}, {0, 2}]) ++
      io_lib:format("---------~n", []) ++
	print_row(Board, [{1, 0}, {1, 1}, {1, 2}]) ++
	  io_lib:format("---------~n", []) ++
	    print_row(Board, [{2, 0}, {2, 1}, {2, 2}]).

print_row(_, []) -> io_lib:format("~n", []);
print_row(Board, [H | T]) ->
    Seperator = fun ([]) -> "";
		    (_) -> " | "
		end,
    io_lib:format("~s", icon(H, Board) ++ Seperator(T) ++ print_row(Board, T)).

icon(_, []) -> " ";
icon({PX, PY}, [{X, Y, P} | T]) ->
    Icon = fun (1) -> "X";
	       (0) -> "O"
	   end,
    if {PX, PY} == {X, Y} -> Icon(P);
       true -> icon({PX, PY}, T)
    end.

next_turn([]) -> 0;
next_turn([{_, _, 1} | _]) -> 0;
next_turn([{_, _, 0} | _]) -> 1.

board_move(X, _, _) when X < 0 -> {not_open};
board_move(X, _, _) when X > 2 -> {not_open};
board_move(_, Y, _) when Y < 0 -> {not_open};
board_move(_, Y, _) when Y > 2 -> {not_open};
board_move(X, Y, Board) ->
    lists:append([{X, Y, next_turn(Board)}]).

current_player([{_, _, Player} | _]) -> Player.
