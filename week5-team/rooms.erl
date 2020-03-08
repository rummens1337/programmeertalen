%%%====================================================================
% @author Michel Rummens <13108093>
% @author Thomas vos <12829501>
% @copyright 2020 M. Rummens, T vos.
% @version 9000
% @title Welcome to the `Dots & Boxes' application!
% @doc `Dots & Boxes' is a simulated game versus a computer.
%%%====================================================================

-module(rooms).
-behaviour(gen_server).

%%% API
-export([start_link/0, start_link/1, new_grid/2,
        get_wall/3, has_wall/4, add_wall/4, show_vlines/2, show_hlines/2,
        print_grid/1, get_cell_walls/2, get_all_walls/2, get_open_spots/1,
        choose_random_wall/1, build_random_wall/1, get_open_cell_walls/3,
        get_completable_walls/1, get_completable_wall/1, build_wall/1]).

%%% Genserver callbacks
-export([init/1, handle_call/3, handle_cast/2,
         terminate/2, code_change/3]).

%%% Internal functions - testing purposes.
-export([get_walls/3]).

%%%====================================================================
%%% API
%%%====================================================================

% @doc      Calls the start_link/1 func with [].
% @param    None.
% @returns  None.
start_link() ->
    start_link([]).

% @doc      Calls the start_link/1 func with [].
% @param    Grid to be used for initialization.
% @returns  {ok, <PID of genserver>} on success.
start_link(Grid) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Grid, []).

% @doc      Initializes a new grid.
% @param    Int Width and int Height, creating a Width x Height grid.
% @returns  {Width, Height, list of added walls (empty at first)}.
new_grid(Width,Height) ->
   {Width, Height, []}.

% @doc      Gets the north, east, south or west wall of a cell.
% @param    Int X & int Y as the coordinates of the cell,
%           atom Dir as the directory.
% @returns  The wall is represented as being between two coordinates (tuples).
get_wall(X,Y,Dir) when Dir == north ->
    {{X,Y-1}, {X,Y}};
get_wall(X,Y,Dir) when Dir == east ->
    {{X,Y}, {X+1,Y}};
get_wall(X,Y,Dir) when Dir == south ->
    {{X,Y}, {X,Y+1}};
get_wall(X,Y,Dir) when Dir == west ->
    {{X-1,Y}, {X,Y}}.

% @doc      Looks if the specified wall exists.
% @param    Int X & int Y as coordinates, atom Dir as the directory, Grid as the
%           the grid {width, height, walls}.
% @returns  Atom true if the specified wall exists, otherwise the atom false.
has_wall(X, Y, Dir, Grid) ->
    lists:member(get_wall(X,Y,Dir),element(3,Grid)).

% @doc      Adds a wall to the grid.
% @param    Int X & int Y as coordinates, atom Dir as the directory, Grid as the
%           the grid {width, height, walls}.
% @returns  A new grid, with the added wall.
add_wall(X, Y, Dir, Grid) ->
        {W,H,List} = Grid,
    {W,H,[get_wall(X,Y,Dir) | List]}.

% @doc      Calls vlines_counter with the specified argument, and returns the
%           final string.
% @param    Int Row as rownumber, Grid as the grid {width, height, walls}.
% @returns  StringVlines as the formatted string of the vertical lines of Row.
show_vlines(Row, Grid) ->
    Max = element(1, Grid),
    StringVlines = vlines_counter(Grid, Row, 0, Max, ""),
    StringVlines.

% @doc      Calls hlines_counter with the specified argument, and returns the
%           final string.
% @param    Int Row as rownumber, Grid as the grid {width, height, walls}.
% @returns  StringHlines as the formatted string of the horizontal lines of Row.
show_hlines(Row, Grid) ->
    Max = element(1, Grid),
    StringHlines = hlines_counter(Grid, Row, 0, Max, ""),
    StringHlines.

% @doc      Calls print_grid/3 with the correct arguments.
% @param    Grid as the grid {width, height, walls}.
% @returns  None.
print_grid(Grid) ->
    Height = element(2, Grid),
    print_grid(Grid, 0, Height).

% @doc      Gets all the walls from a cell.
% @param    Int X & int Y as coordinates.
% @returns  Walls as the list of all adjacent walls of a cell.
get_cell_walls(X, Y) ->
    Walls = [get_wall(X, Y, north)] ++ [get_wall(X, Y, east)] ++
               [get_wall(X, Y, south)] ++ [get_wall(X, Y, west)],
    Walls.

% @doc      Calculates a list with all possible walls in the grid.
% @param    Int W as the width & int H as the height of the grid.
% @returns  A list of all possible walls in a grid, without duplicates.
get_all_walls(W, H) ->
    Walls = get_all_walls(W, H, 0, []),
    lists:usort(Walls).

% @doc      Calculates a list of open spots in the grid.
% @param    Grid as the grid {width, height, walls}.
% @returns  A list of all open spots (no wall) in a grid.
get_open_spots(Grid) ->
    get_all_walls(element(1, Grid), element(2, Grid)) -- element(3, Grid).

% @doc      Chooses a random open wall from the grid.
% @param    Grid as the grid {width, height, walls}.
% @returns  A random open wall.
choose_random_wall(Grid) ->
    Open = get_open_spots(Grid),
    case(Open) of
        []  -> [];
        _ -> lists:nth(rand:uniform(length(Open)), Open)
    end.

% @doc      Builds a random wall in the grid.
% @param    Grid as the grid {width, height, walls}.
% @returns  The new grid, with the random wall added.
build_random_wall(Grid) ->
    {W, H, List} = Grid,
    Wall = choose_random_wall(Grid),
    case(Wall) of
        [] -> no_build;
        _  -> {W,H,lists:usort([Wall | List])}
        end.

% @doc      Computes a list of open walls for a given cell (X,Y).
% @param    Int X & int Y as coordinates, Grid as the grid
%           {width, height, walls}.
% @returns  A list of open walls for a given cell.
get_open_cell_walls(X,Y,Grid) ->
    Dirs = [get_wall(X,Y, north),get_wall(X,Y, east),
            get_wall(X,Y, south),get_wall(X,Y, west)],
    lists:sort([Elm || Elm <- Dirs, not(lists:member(Elm, element(3,Grid)))]).

% Returns a list of completable walls from the Grid.
get_completable_walls(Grid) ->
    {W,H,_List} = Grid,
   lists:flatten([get_open_cell_walls(X,Y, Grid)
                    || {X,Y} <- [XY
                    || XY <- lists:flatten([lists:zip(lists:duplicate(W, N), lists:seq(0, H-1))
                    || N <- lists:seq(0, W-1)])], length(get_open_cell_walls(X,Y, Grid)) == 1]).

get_completable_wall(Grid) ->
    Walls = get_completable_walls(Grid),
    case(Walls) of 
        [] -> false;
        _  -> [H|_T] = Walls, H
    end.

build_wall(Grid) ->
    {W,H,List} = Grid,
    Completable = get_completable_wall(Grid),
    case(Completable) of
        false -> build_random_wall(Grid);
        _     -> {W,H,lists:usort([Completable | List])}
        end.

%%%====================================================================
%%% Genserver callbacks
%%%====================================================================

% @doc      Initializes the grid (starting the game).
% @param    Grid as the grid {width, height, walls}.
% @returns  {ok, the initialized grid}.
init(Grid) -> {ok, Grid}.

% @doc      Terminates the game.
% @param    Atom terminate, request _From, current state of the grid as State.
% @returns  {atom stop, atom normal, ok, current state of the grid}.
handle_call(terminate, _From, State) ->
    {stop, normal, ok, State}.

% @doc      Restarts the board.
% @param    Atom restart, _State as the current state of the Grid.
% @returns  {atom noreply, empty grid}.
handle_cast(restart, _State) ->
    {noreply, []}.

% @doc      Terminates the gen_server.
% @param    Atom normal, the current state of the Grid.
% @returns  ok.
terminate(normal, _) ->
    ok.

% @doc      Updates the internal state of the gen_server.
% @param    The old VSN, the current state of the server, additional info.
% @returns  {ok, new state}.
code_change(_Old, State, _Extra) ->
    {ok, State}.

%%%====================================================================
%%% Internal functions
%%%====================================================================

% @doc      Calls vlines_string with the correct arguments based on the state
%           of the counter.
% @param    Grid as the grid {width, height, walls}, int Row as the rownumber,
%           int Counter as the amount of iterations, int Max as the maximum
%           number of iterations, String as the formatted string.
% @returns  The final formatted string of the vertical lines of a row.
vlines_counter(Grid, Row, Counter, Max, String) ->
    case(Counter) of
        Max -> vlines_string(max, Grid, Row, Counter, String);
        _   -> vlines_string(notmax, Grid, Row, Counter, Max, String)
    end.

% @doc      Formats a vertical wall to a string and adds it to the current
%           string. If max is specified, the last part of the string will
%           contain a ~n and the recursion will stop.
% @param    The atom max/notmax,
%           grid as the grid {width, height, walls}, int Row as the rownumber,
%           int Counter as the amount of iterations, int Max as the maximum
%           number of iterations, String as the formatted string.
% @returns  Max: the final formatted string of the vertical lines of a row.
%           Notmax: calls the function vlines_counter again with the new
%           String.
vlines_string(max, Grid, Row, Counter, String) ->
    NewCounter = Counter - 1,
    Wall = has_wall(NewCounter, Row, east, Grid),

    case(Wall) of
        true  -> NewString = String ++ "|~n",
              NewString;
        false -> NewString = String ++ " ~n",
              NewString
    end.

vlines_string(notmax, Grid, Row, Counter, Max, String) ->
    Wall = has_wall(Counter, Row, west, Grid),

    case(Wall) of
    true  -> NewString = String ++ "|  ",
              NewCounter = Counter + 1,
              vlines_counter(Grid, Row, NewCounter, Max, NewString);
    false -> NewString = String ++ "   ",
              NewCounter = Counter + 1,
              vlines_counter(Grid, Row, NewCounter, Max, NewString)

    end.

% @doc      Calls hlines_string with the correct arguments based on the state
%           of the counter.
% @param    Grid as the grid {width, height, walls}, int Row as the rownumber,
%           int Counter as the amount of iterations, int Max as the maximum
%           number of iterations, String as the formatted string.
% @returns  The final formatted string of the horizontal lines of a row.
hlines_counter(Grid, Row, Counter, Max, String) ->
    case(Counter) of
        Max -> hlines_string(max, String);
        _   -> hlines_string(notmax, Grid, Row, Counter, Max, String)
    end.

% @doc      Formats a horizontal wall to a string and adds it to the current
%           string. If max is specified, the last part of the string will
%           contain a ~n and the recursion will stop.
% @param    The atom max/notmax,
%           grid as the grid {width, height, walls}, int Row as the rownumber,
%           int Counter as the amount of iterations, int Max as the maximum
%           number of iterations, String as the formatted string.
% @returns  Max: the final formatted string of the horizontal lines of a row.
%           Notmax: calls the function hlines_counter again with the new
%           String.
hlines_string(max, String) ->
    NewString = String ++ "+~n",
    NewString.

hlines_string(notmax, Grid, Row, Counter, Max, String) ->
    Wall = has_wall(Counter, Row, north, Grid),

    case(Wall) of
    true  -> NewString = String ++ "+--",
             NewCounter = Counter + 1,
             hlines_counter(Grid, Row, NewCounter, Max, NewString);
    false -> NewString = String ++ "+  ",
             NewCounter = Counter + 1,
             hlines_counter(Grid, Row, NewCounter, Max, NewString)
    end.

% @doc      Prints the full string-formatted grid.
% @param    Grid as the grid {width, height, walls}, int Counter as the current
%           rownumber, int Height as the total height of the grid.
% @returns  None.
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

% @doc      Gets a list of all walls.
% @param    int W as width, int H as height, int CountV as the current
%           rownumber, list AllWalls as a list with all walls (Empty on first
%           call).
% @returns  A list of all possible walls, with duplicates.
get_all_walls(W, H, CountV, AllWalls) ->
    case(CountV) of
        H -> AllWalls;
        _ -> Walls = get_walls_row(W, H, 0, CountV, AllWalls),
             NewCounter = CountV + 1,
             get_all_walls(W, H, NewCounter, Walls)

    end.

% @doc      Gets a list of all walls adjacent to all cells in a row.
% @param    int W as width, int H as height, CountH as the current
%           cellnumber, Y as the rownumber, list AllWalls as a list with all
%           walls (Empty on first call).
% @returns  A list of all possible walls adjacent to all cells in a row,
%           with duplicates.
get_walls_row(W, H, CountH, Y, AllWalls) ->
    case(CountH) of
        W -> AllWalls;
        _ -> Walls = AllWalls ++ get_cell_walls(CountH, Y),
             NewCounter = CountH + 1,
             get_walls_row(W, H, NewCounter, Y, Walls)
    end.

% retrieve_completeable_walls(Grid, Completable, Coordinates) ->
%     case(Iterator) of
%         0 -> Completable;
%         _ -> get
