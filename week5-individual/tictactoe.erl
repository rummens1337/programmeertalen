-module(tictactoe).
-behaviour(gen_server).

%% API
-export([transpose/1, check_rows/1]).
-export([start_link/0, start_link/1, print_board/0, print_board/1, show_board/1,
     restart/0, restart/1, move/2, is_finished/0, get_board/0]).

%% Genserver callbacks
-export([init/1, handle_call/3, handle_cast/2,
         terminate/2, code_change/3]).


-define(MAX_MOVES, 9).
-define(MIN_LENGTH, 9).


%%%====================================================================
%%% API
%%%====================================================================

% Starts with an empty board.
start_link() ->
    start_link([]).

% Starts with a preconfigured board.
start_link(Board) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Board, []).

init(Board) -> 
    {ok, Board}.

%%% TODO: implement these functions.
% tictactoe:start_link(), tictactoe:restart().
restart() ->
    gen_server:call(?MODULE, restart).

% tictactoe:start_link(), tictactoe:restart([[1,0,0],[1,2,0]]).
restart(Board) ->
    gen_server:call(?MODULE, {restart, Board}).

move(X,Y) ->
    Board = get_board(),
    
    % gen_server:call(?MODULE, {show_board, {X,Y}}).
    ok.

% If 9 moves are made, no more possibilities remain.
is_finished() ->
    Board = get_board(),
    _Length = length(Board),

    Rows = check_rows(lists:keysort(1, Board)), % Check rows
    Columns = check_rows(lists:keysort(2, Board)), %Check columns
    {Rows,Columns}.
    % check_rows(lists:keysort(2, Board)). % Check columns
    % ((Length > ?MIN_LENGTH) and (Length < (?MAX_MOVES + 1))) and (Length / ?MAX_MOVES == 0).

check_rows([]) -> false; % no combinations of 3 found.
check_rows([{X1,Y1,P1},{X2,Y2,P2}, {X3,Y3,P3} | Tail]) ->
    if
        (((P1 == P2) and (P2 == P3))) and 
        ((X1 == X2) and X2 == X3) or
        ((Y1 == Y2) and Y2 == Y3)  ->
            true;
        true -> 
            % {P1}
            check_rows(Tail)
    end.

get_board() ->
    gen_server:call(?MODULE, get_board).

show_board(Board) ->
    % Print board formatted.
    ok.

%%% TODO: Add the required calls.
handle_call({test, Value}, _From, Board) ->
    NewState = [Value | Board],
    {reply, NewState, NewState};

% Set the new state to an empty board.
handle_call(restart, _From, _Board) ->
    {reply, [], []};

% Set the new board as current state.
handle_call({restart, NewBoard }, _From, _Board) ->
    {reply, NewBoard, NewBoard};

handle_call(get_board, _From, Board) ->
    Lel = [
        {0,0,2},{0,1,1},{0,2,1},
        {1,0,2},{1,1,1},{1,2,1},
        {2,0,1},{2,1,1},{2,2,2}
    ],
    {reply, Lel, Lel}.
    % {action, return_value, new state}

handle_cast(restart, _State) ->
    {noreply, []}.

terminate(normal, _) ->
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.


%%%====================================================================
%%% internal functions
%%%====================================================================


% This is just a helper for in the REPL.
print_board() ->
    print_board(get_board()).

% This allows you to test printing without the server working.
print_board(Board) ->
    io:fwrite(show_board(Board)).

transpose([]) -> [];
transpose([[] | Xss]) -> transpose(Xss);
transpose([[X | Xs] | Xss]) ->
    [[X | [H || [H | T] <- Xss]]
    | transpose([Xs | [T || [H | T] <- Xss]])].