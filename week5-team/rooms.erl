%%%====================================================================
%% author: Michel Rummens
%% studentnumber: 13108093
%%
%% author: Thomas vos
%% studentnumber: 12829501
%%
%% This module is a tic tac toe 
%% for the rooms game.
%%%====================================================================

-module(rooms).
-behaviour(gen_server).

%%% API
-export([start_link/0, start_link/1, new_grid/2,
        get_wall/3, has_wall/4, add_wall/4, show_vlines/2, show_hlines/2,
        print_grid/1, get_cell_walls/2, get_all_walls/2, get_open_spots/1]).

%%% Genserver callbacks
-export([init/1, handle_call/3, handle_cast/2,
         terminate/2, code_change/3]).

%%% Internal functions - testing purposes.
-export([get_wall/3, has_wall/4, wall_add/4, get_walls/3]).

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
   {Width, Height, []}.

get_wall(X,Y,Dir) when Dir == north ->
    {{X,Y-1}, {X,Y}};

get_wall(X,Y,Dir) when Dir == east ->
    {{X,Y}, {X+1,Y}};

get_wall(X,Y,Dir) when Dir == south ->
    {{X,Y}, {X,Y+1}};

get_wall(X,Y,Dir) when Dir == west ->
    {{X-1,Y}, {X,Y}}.

has_wall(X, Y, Dir, Grid) ->
    lists:member(get_wall(X,Y,Dir),element(3,Grid)).

add_wall(X, Y, Dir, Grid) ->
        {W,L,List} = Grid,
    {W,L,[get_wall(X,Y,Dir) | List]}.

% Prints all vertical lines of a row.
show_vlines(Row, Grid) ->
    Max = element(1, Grid),
    StringVlines = vlines_counter(Grid, Row, 0, Max, ""),
    StringVlines.

% Prints all horizontal lines of a row.
show_hlines(Row, Grid) ->
    Max = element(1, Grid),
    StringHlines = hlines_counter(Grid, Row, 0, Max, ""),
    StringHlines.

% Prints the grid.
print_grid(Grid) ->
    Height = element(2, Grid),
    print_grid(Grid, 0, Height).

% Gets all the walls from a cell.
get_cell_walls(X, Y) ->
    Walls = [get_wall(X, Y, north)] ++ [get_wall(X, Y, east)] ++
               [get_wall(X, Y, south)] ++ [get_wall(X, Y, west)],
    Walls.

get_all_walls(W, H) ->
    Walls = get_all_walls(W, H, 0, 0, []),
    lists:usort(Walls).

get_open_spots(Grid) ->
    get_all_walls(element(1, Grid), element(2, Grid)) -- element(3, Grid).

%%%====================================================================
%%% Genserver callbacks
%%%====================================================================

init(Board) -> {ok, Board}.

handle_call(terminate, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(restart, _State) ->
    {noreply, []}.

terminate(normal, _) ->
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.

%%%====================================================================
%%% Internal functions
%%%====================================================================

% Adds a wall to the grid.
wall_add(X,Y,Dir,Grid) ->
    {W,L,List} = Grid,
    {W,L,[get_wall(X,Y,Dir) | List]}.

% Formats vertical lines for given row.
vlines_counter(Grid, Row, Counter, Max, String) ->
    case(Counter) of
        Max -> vlines_string(max, Grid, Row, Counter, String);
        _   -> vlines_string(notmax, Grid, Row, Counter, Max, String)
    end.

vlines_string(max, Grid, Row, Counter, String) ->
    NewCounter = Counter - 1,
    Wall = has_wall(Row, NewCounter, east, Grid),

    case(Wall) of
        true  -> NewString = String ++ "|~n",
              NewString;
        false -> NewString = String ++ " ~n",
              NewString
    end.

vlines_string(notmax, Grid, Row, Counter, Max, String) ->
    Wall = has_wall(Row, Counter, west, Grid),

    case(Wall) of
    true  -> NewString = String ++ "|  ",
              NewCounter = Counter + 1,
              vlines_counter(Grid, Row, NewCounter, Max, NewString);
    false -> NewString = String ++ "   ",
              NewCounter = Counter + 1,
              vlines_counter(Grid, Row, NewCounter, Max, NewString)

    end.

% Formats horizontal lines for given row.
hlines_counter(Grid, Row, Counter, Max, String) ->
    case(Counter) of
        Max -> hlines_string(max, String);
        _   -> hlines_string(notmax, Grid, Row, Counter, Max, String)
    end.

hlines_string(max, String) ->
    NewString = String ++ "+~n",
    NewString.


hlines_string(notmax, Grid, Row, Counter, Max, String) ->
    Wall = has_wall(Row, Counter, south, Grid),

    case(Wall) of
    true  -> NewString = String ++ "+  ",
             NewCounter = Counter + 1,
             hlines_counter(Grid, Row, NewCounter, Max, NewString);
    false -> NewString = String ++ "+--",
             NewCounter = Counter + 1,
             hlines_counter(Grid, Row, NewCounter, Max, NewString)
    end.

% Prints the full string-formatted grid.
print_grid(Grid, Counter, Height) ->
    case(Counter) of
        Height -> RowHor = show_hlines(Counter, Grid),
                  io:fwrite(RowHor);
        _Rest  -> RowHor = show_hlines(Counter, Grid),
                  RowVer = show_vlines(Counter, Grid),
                  io:fwrite(RowHor),
                  io:fwrite(RowVer),
                  NewCounter = Counter + 1,
                  print_grid(Grid, NewCounter, Height)

    end.

% Gets a list of all walls.
get_all_walls(W, H, 0, CountV, AllWalls) ->
    case(CountV) of
        H -> AllWalls;
        _ -> Walls = get_walls_row(W, H, 0, CountV, AllWalls),
             NewCounter = CountV + 1,
             get_all_walls(W, H, 0, NewCounter, Walls)

    end.

get_walls_row(W, H, CountH, Y, AllWalls) ->
    case(CountH) of
        W -> AllWalls;
        _ -> Walls = AllWalls ++ get_cell_walls(CountH, Y),
             NewCounter = CountH + 1,
             get_walls_row(W, H, NewCounter, Y, Walls)
    end.

% Get walls for vertical or horizontal. Not needed for the printgrid.
get_walls(vertical, Row, Grid) ->
    Walls = [{{X1,Y1},{X2,Y2}} || {{X1,Y1},{X2,Y2}} <- Grid, X2 == Row],
    Walls;

get_walls(horizontal, Row, Grid) ->
    Walls = [{{X1,Y1},{X2,Y2}} || {{X1,Y1},{X2,Y2}} <- Grid, Y2 == Row],
    Walls.

