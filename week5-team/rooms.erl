-module(rooms).
-behaviour(gen_server).

%%% API
-export([start_link/0, start_link/1, new_grid/2,
        get_wall/3, has_wall/4, add_wall/4, show_vlines/2]).

%%% Genserver callbacks
-export([init/1, handle_call/3, handle_cast/2,
         terminate/2, code_change/3]).

%%% Internal functions - testing purposes.
-export([wall_get/3, wall_has/4, wall_add/4, get_walls/3]).

%%%====================================================================
%%% API
%%%====================================================================

% Starts with an empty board.
start_link() ->
    start_link([]).

% Starts with a preconfigured board.
start_link(Grid) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Grid, []).

new_grid(Width,Height) ->
    gen_server:call(?MODULE, {new_grid, Width, Height}).

get_wall(X, Y, Dir) ->
    gen_server:call(?MODULE, {get_wall, X, Y, Dir}).

has_wall(X, Y, Dir, Grid) ->
    gen_server:call(?MODULE, {has_wall, X, Y, Dir, Grid}).

add_wall(X, Y, Dir, Grid) ->
    gen_server:call(?MODULE, {add_wall, X, Y, Dir, Grid}).

% gen_server:call(?MODULE, {show_vlines, Row}). % onnodig hier aangezien je de grid meekrijgt.

% prints all vertical lines of a row.
show_vlines(Row, Grid) ->
    VerticalWalls =  get_walls(vertical, Row, element(3, Grid)),
    Max = element(1, Grid),
    StringVlines = vlines_counter(VerticalWalls, 0, Max, ""),
    io_lib:format("~p", [StringVlines]).

% prints all horizontal lines of a row.
show_hlines(Row, Grid) ->
    HorizontalWalls =  get_walls(horizontal, Row, element(3, Grid)),
    Max = element(1, Grid),
    StringHlines = hlines_counter(HorizontalWalls, 0, Max, ""),
    io_lib:format("~p", [StringHlines]).

% prints the grid.
print_grid(Grid) ->
    ok.


%%%====================================================================
%%% Genserver callbacks
%%%====================================================================

init(Board) -> {ok, Board}.

handle_call(terminate, _From, State) ->
    {stop, normal, ok, State};

handle_call({new_grid, Width, Height}, _From, _State) ->
    Grid = {Width, Height, []},
    {reply, Grid, Grid};

handle_call({get_wall, X, Y, Dir}, _From, _State) ->
    Wall = wall_get(X,Y,Dir),
    {reply, Wall, Wall};

handle_call({has_wall, X, Y, Dir, Grid}, _From, State) ->
    HasWall = wall_has(X,Y,Dir,Grid),
    {reply, HasWall, State};

handle_call({add_wall, X, Y, Dir, Grid}, _From, _State) ->
    Wall = wall_add(X,Y,Dir,Grid),
    {reply, Wall, Wall}.

% No gen_server needed for string function.

% handle_call({show_vlines, Row, Grid}, _From, _State) -> % unfinished
%     VLine = vlines_show(Row, Grid),
%     {reply, VLine, VLine};

% handle_call({show_hlines, Row, Grid}, _From, _State) -> % unfinished
%     HLine = hlines_show(Row, Grid),
%     {reply, HLine, HLine};

% handle_call({print_grid, Grid}, _From, _State) -> % unfinished
%     Grid = print_grid(Grid),
%     {reply, Grid, Grid}.

% Action(reply/noreply), Response, Newstate

handle_cast(restart, _State) ->
    {noreply, []}.

terminate(normal, _) ->
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.

%%%====================================================================
%%% Internal functions
%%%====================================================================

% Gets a wall at a certain X,Y,Dir position.

wall_get(X,Y,Dir) when Dir == north ->
    {{X,Y-1}, {X,Y}};

wall_get(X,Y,Dir) when Dir == east ->
    {{X,Y}, {X+1,Y}};

wall_get(X,Y,Dir) when Dir == south ->
    {{X,Y}, {X,Y+1}};

wall_get(X,Y,Dir) when Dir == west ->
    {{X-1,Y}, {X,Y}}.

% Checks if wall is present in the Grid.
wall_has(X,Y,Dir,Grid) ->
    lists:member(wall_get(X,Y,Dir),element(3,Grid)).

% Adds a wall to the grid.
wall_add(X,Y,Dir,Grid) ->
    {W,L,List} = Grid,
    {W,L,[wall_get(X,Y,Dir) | List]}.

% Format vertical lines for given row.
vlines_counter(VWalls, Counter, Max, String) ->
    case(Counter) of
        Max -> vlines_string(max, VWalls, Counter, Max, String);
        _   -> vlines_string(notmax, VWalls, Counter, Max, String)
    end.

vlines_string(max, VWalls, Counter, Max, String) ->
    VWall = [{{X1,Y1},{X2,Y2}} || {{X1,Y1},{X2,Y2}} <- VWalls, (X1 == Counter,
    X2 == Counter - 1) ; (X1 == Counter - 1, X2 == Counter)],

    case(VWall) of
        [] -> NewString = String ++ " ",
              NewString;
        _  -> NewString = String ++ "|",
              NewString
    end.

vlines_string(notmax, VWalls, Counter, Max, String) ->
    VWall = [{{X1,Y1},{X2,Y2}} || {{X1,Y1},{X2,Y2}} <- VWalls, (X1 == Counter,
    X2 == Counter - 1) ; (X1 == Counter - 1, X2 == Counter)],

    case(VWall) of
        [] -> NewString = String ++ "   ",
              NewCounter = Counter + 1,
              vlines_counter(VWalls, NewCounter, Max, NewString);
        _  -> NewString = String ++ "|  ",
              NewCounter = Counter + 1,
              vlines_counter(VWalls, NewCounter, Max, NewString)
    end.

% Format horizontal lines for given row.
hlines_counter(HWalls, Counter, Max, String) ->
    ok.

hlines_string(max, VWalls, Counter, Max, String) -> ok;

hlines_string(_, VWalls, Counter, Max, String) -> ok.

% Get walls for  vertical or horizontal.
get_walls(vertical, Row, Grid) ->
    Walls = [{{X1,Y1},{X2,Y2}} || {{X1,Y1},{X2,Y2}} <- Grid, X2 == Row],
    Walls;

get_walls(horizontal, Row, Grid) ->
    Walls = [{{X1,Y1},{X2,Y2}} || {{X1,Y1},{X2,Y2}} <- Grid, Y2 == Row],
    Walls.

% % This is just a helper for in the REPL.
% print_board() ->
%     print_board(get_board()).

% % This allows you to test printing without the server working.
% print_board(Board) ->
%     io:fwrite(show_board(Board)).

% help_me(Animal) ->
% Talk = if Animal == cat  -> "meow";
% Animal == beef -> "mooo";
% Animal == dog  -> "bark";
% Animal == tree -> "bark";
% true -> "fgdadfgna"
% end,
% {Animal, "says " ++ Talk ++ "!"}.
