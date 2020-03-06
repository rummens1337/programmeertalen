-module(tictactoe).

-behaviour(gen_server).

-include_lib("stdlib/include/qlc.hrl").

%% API
-export([check_rows/1, current_player/1, valid_move/4]).

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

restart(Board) ->
    gen_server:call(?MODULE, {restart, Board}).

get_board() -> gen_server:call(?MODULE, get_board).

%%%====================================================================
%%% genserver
%%%====================================================================

handle_call({test, Value}, _From, Board) ->
    NewState = [Value | Board], {reply, NewState, NewState};
handle_call(get_board, _From, Board) ->
    {reply, Board, Board};
handle_call(restart, _From, _Board) -> {reply, [], []};
handle_call({restart, NewBoard}, _From, _Board) ->
    {reply, NewBoard, NewBoard};
handle_call({move, Move}, _From, Board) ->
    NewBoard = [Move | Board], {reply, ok, NewBoard}.

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
check_rows(List) when length(List) < 3 -> false;
check_rows([{X1, Y1, P1}, {X2, Y2, P2}, {X3, Y3, P3}
	    | Tail]) ->
    if (P1 == P2) and (P2 == P3) and
	 ((X1 == X2) and (X2 == X3) or
	    (Y1 == Y2) and (Y2 == Y3)) ->
	   true;
       true -> check_rows(Tail)
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
    ok.

move(X, Y)
    when (X < (?MIN_MOVES)) or (X > (?MAX_MOVES)) or
	   (Y < (?MIN_MOVES))
	   or (Y > (?MAX_MOVES)) ->
    not_valid;

move(X, Y) ->
    % {?PLAYERS, X, Y, Board}.
    Valid = valid_move(?PLAYERS, X, Y, get_board()),
    Val = if Valid ->
		 gen_server:call(?MODULE, {move, {X, Y, 1}});
	     true -> not_open
	  end,
    Finished = is_finished(),
    Player = current_player(lists:nthtail(length(get_board())-1, get_board())),
          if
			    Val == not_open -> not_open;
			    true ->
			        if
			            Finished -> {won, Player};
			            true ->
			                ok
			            end
			        end.

    % gen_server:call(?MODULE, {move, {X,Y,1}}).

valid_move([], _X, _Y, _Board) -> true;
valid_move([P | Players], X, Y, Board) ->
    NotValid = lists:member({X, Y, P}, Board),
    if NotValid -> false;
       true -> valid_move(Players, X, Y, Board)
    end.

current_player([{_, _, Player} | _]) -> Player.
