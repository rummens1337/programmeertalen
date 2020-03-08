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

%%%====================================================================
%%% API
%%%====================================================================

start(Width, Height) ->
    Grid = new_grid(Width, Height),

    <<S1:32, S2:32, S3:32>> = crypto:strong_rand_bytes(12),
    RandSeed = rand:seed(exs1024,{S1, S2, S3}),

    spawn(rooms, start, [RandSeed])

    register(cpu2, spawn(rooms, cpu1, [])),

    spawn(rooms, cpu1, [0,0, get_completable_walls(Grid), new_grid(3,3)]).

cpu1() ->

cpu2() ->

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
% @param    Int Width: width of the grid.
% @param    Int Height: height of the grid.
% @returns  A tuple with the width, height and empty list for walls of the grid.
new_grid(Width,Height) ->
   {Width, Height, []}.

% @doc      Gets the north, east, south or west wall of a cell.
% @param    Int X: x-coordinate of the cell.
% @param    Int Y: y-coordinate of the cell.
% @param    Atom Dir: direction.
% @returns  The wall, represented as being between two coordinates -> {{0,0},{-1,0}}.
get_wall(X,Y,Dir) when Dir == north ->
    {{X,Y-1}, {X,Y}};
get_wall(X,Y,Dir) when Dir == east ->
    {{X,Y}, {X+1,Y}};
get_wall(X,Y,Dir) when Dir == south ->
    {{X,Y}, {X,Y+1}};
get_wall(X,Y,Dir) when Dir == west ->
    {{X-1,Y}, {X,Y}}.

% @doc      Checks if the specified wall exists.
% @param    Int X: x-coordinate of the cell.
% @param    Int Y: y-coordinate of the cell.
% @param    Atom Dir: direction.
% @param    Tuple Grid: the grid {width, height, list of walls}.
% @returns  Atom true if the specified wall exists, otherwise the atom false.
has_wall(X, Y, Dir, Grid) ->
    lists:member(get_wall(X,Y,Dir),element(3,Grid)).

% @doc      Adds a wall to the grid.
% @param    Int X: x-coordinate of the cell.
% @param    Int Y: y-coordinate of the cell.
% @param    Atom Dir: direction.
% @param    Tuple Grid: the grid {width, height, list of walls}.
% @returns  A new grid, with the added wall.
add_wall(X, Y, Dir, Grid) ->
        {W,H,List} = Grid,
    {W,H,[get_wall(X,Y,Dir) | List]}.

% @doc      Calls vlines_counter with the specified argument, and returns the
%           final string.
% @param    Int Row: rownumber.
% @param    Tuple Grid: the grid {width, height, list of walls}.
% @returns  Formatted string of the vertical lines of Row.
show_vlines(Row, Grid) ->
    Max = element(1, Grid),
    StringVlines = vlines_counter(Grid, Row, 0, Max, ""),
    StringVlines.

% @doc      Calls hlines_counter with the specified argument, and returns the
%           final string.
% @param    Int Row: rownumber.
% @param    Tuple Grid: the grid {width, height, list of walls}.
% @returns  Formatted string of the horizontal lines of Row.
show_hlines(Row, Grid) ->
    Max = element(1, Grid),
    StringHlines = hlines_counter(Grid, Row, 0, Max, ""),
    StringHlines.

% @doc      Calls print_grid/3 with the correct arguments.
% @param    Tuple Grid: the grid {width, height, list of walls}.
% @returns  Calls print_grid/3, which prints the grid.
print_grid(Grid) ->
    Height = element(2, Grid),
    print_grid(Grid, 0, Height).

% @doc      Finds all the walls for specific X,Y cell.
% @param    Int X: x-coordinate of the cell.
% @param    Int Y: y-coordinate of the cell.
% @returns  List of all adjacent walls of a cell.
get_cell_walls(X, Y) ->
    Walls = [get_wall(X, Y, north)] ++ [get_wall(X, Y, east)] ++
               [get_wall(X, Y, south)] ++ [get_wall(X, Y, west)],
    Walls.

% @doc      Creates a list with all possible walls in the grid.
% @param    Int W: width of the grid.
% @param    Int H: the height of the grid.
% @returns  A list of all possible walls in a grid, without duplicates.
get_all_walls(W, H) ->
    Walls = get_all_walls(W, H, 0, []),
    lists:usort(Walls).

% @doc      Creates a list of open spots in the grid.
% @param    Tuple Grid: the grid {width, height, list of walls}.
% @returns  A list of all open spots (no walls) in a grid.
get_open_spots(Grid) ->
    get_all_walls(element(1, Grid), element(2, Grid)) -- element(3, Grid).

% @doc      Chooses a random open wall from the grid.
% @param    Tuple Grid: the grid {width, height, list of walls}.
% @returns  A random open wall.
choose_random_wall(Grid) ->
    Open = get_open_spots(Grid),
    case(Open) of
        []  -> [];
        _ -> lists:nth(rand:uniform(length(Open)), Open)
    end.

% @doc      Builds a random wall in the grid.
% @param    Tuple Grid: the grid {width, height, list of walls}.
% @returns  The new grid, with the random wall added.
build_random_wall(Grid) ->
    {W, H, List} = Grid,
    Wall = choose_random_wall(Grid),
    case(Wall) of
        [] -> no_build;
        _  -> {W,H,lists:usort([Wall | List])}
        end.

% @doc      Computes a list of open walls for a given cell (X,Y).
% @param    Int X: x-coordinate of the cell.
% @param    Int Y: y-coordinate of the cell.
% @param    Tuple Grid: the grid {width, height, list of walls}.
% @returns  A list of open walls for a given cell.
get_open_cell_walls(X,Y,Grid) ->
    Dirs = [get_wall(X,Y, north),get_wall(X,Y, east),
            get_wall(X,Y, south),get_wall(X,Y, west)],
    lists:sort([Elm || Elm <- Dirs, not(lists:member(Elm, element(3,Grid)))]).

% @doc      Computes a list of all the completable walls of a grid.
% @param    Tuple Grid: the grid {width, height, list of walls}.
% @returns  A list of completable walls.
get_completable_walls(Grid) ->
    {W,H,_List} = Grid,
   lists:flatten([get_open_cell_walls(X,Y, Grid)
                    || {X,Y} <- [XY
                    || XY <- lists:flatten([lists:zip(lists:duplicate(W, N), lists:seq(0, H-1))
                    || N <- lists:seq(0, W-1)])], length(get_open_cell_walls(X,Y, Grid)) == 1]).

% @doc      Get the first completable wall.
% @param    Tuple Grid: the grid {width, height, list of walls}.
% @returns  The first completable wall on success or false if no completable walls are found.
get_completable_wall(Grid) ->
    Walls = get_completable_walls(Grid),
    case(Walls) of
        [] -> false;
        _  -> [H|_T] = Walls, H
    end.

% @doc      Build a completable wall, otherwise build random wall.
% @param    Tuple Grid: the grid {width, height, list of walls}.
% @returns  Grid with extra element in list of walls.
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
% @param    Tuple Grid: the grid {width, height, list of walls}.
% @returns  {ok, the initialized grid}.
init(Grid) -> {ok, Grid}.

% @doc      Terminates the game.
% @param    Atom terminate.
% @param    Request _From.
% @param    Current state of the grid as State.
% @returns  {atom stop, atom normal, ok, current state of the grid}.
handle_call(terminate, _From, State) ->
    {stop, normal, ok, State}.

% @doc      Restarts the board.
% @param    Atom restart.
% @param    _State: current state of the grid.
% @returns  {atom noreply, empty grid}.
handle_cast(restart, _State) ->
    {noreply, []}.

% @doc      Terminates the gen_server.
% @param    Atom normal.
% @param    The current state of the grid.
% @returns  ok.
terminate(normal, _) ->
    ok.

% @doc      Updates the internal state of the gen_server.
% @param    _Old: old VSN.
% @param    _State: current state of the server.
% @param    _Extra as additional info.
% @returns  {ok, new state}.
code_change(_Old, State, _Extra) ->
    {ok, State}.

%%%====================================================================
%%% Internal functions
%%%====================================================================

% @doc      Calls vlines_string with the correct arguments based on the state
%           of the counter.
% @param    Tuple Grid: the grid {width, height, list of walls}.
% @param    Int Row: rownumber.
% @param    Int Counter: amount of iterations.
% @param    Int Max: maximum number of iterations.
% @param    String: formatted string.
% @returns  The final formatted string of the vertical lines of a row.
vlines_counter(Grid, Row, Counter, Max, String) ->
    case(Counter) of
        Max -> vlines_string(Grid, Row, Counter, String);
        _   -> vlines_string(Grid, Row, Counter, Max, String)
    end.

% @doc      Formats a vertical wall to a string and adds it to the current
%           string. If max is specified, the last part of the string will
%           contain a ~n and the recursion will stop.
% @param    Tuple Grid: the grid {width, height, list of walls}.
% @param    Int Row: rownumber.
% @param    Int Counter: amount of iterations.
% @param    Int Max: maximum number of iterations.
% @param    String: formatted string.
% @returns  Max: the final formatted string of the vertical lines of a row.
%           Notmax: calls the function vlines_counter again with the new
%           String, and therefore forms a recursion with two functions.
vlines_string(Grid, Row, Counter, String) ->
    NewCounter = Counter - 1,
    Wall = has_wall(NewCounter, Row, east, Grid),

    case(Wall) of
        true  -> NewString = String ++ "|~n",
              NewString;
        false -> NewString = String ++ " ~n",
              NewString
    end.

% @doc      Formats a vertical wall to a string and adds it to the current
%           string. The last part of the string will
%           contain a ~n and the recursion will stop.
% @param    Tuple Grid: the grid {width, height, list of walls}.
% @param    Int Row: rownumber.
% @param    Int Counter: amount of iterations.
% @param    Int Max: maximum number of iterations.
% @param    String: formatted string.
% @returns  Notmax: calls the function vlines_counter again with the new
%           String, and therefore forms a recursion with two functions.
vlines_string(Grid, Row, Counter, Max, String) ->
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
% @param    Tuple Grid: the grid {width, height, list of walls}.
% @param    Int Row: rownumber.
% @param    Int Counter: amount of iterations.
% @param    Int Max: maximum number of iterations.
% @param    String: formatted string.
% @returns  Calls the function vlines_counter again with the new String,
%           and therefore forms a recursion with two functions.
hlines_counter(Grid, Row, Counter, Max, String) ->
    case(Counter) of
        Max -> hlines_string(String);
        _   -> hlines_string(Grid, Row, Counter, Max, String)
    end.

% @doc      Places the final plus and ~n on the string with all horizontal walls.
% @param    String: formatted string.
% @returns  The final formatted string with horizontal walls.
hlines_string(String) ->
    NewString = String ++ "+~n",
    NewString.

% @doc      Formats a horizontal wall to a string and adds it to the current
%           string.
% @param    Tuple Grid: the grid {width, height, list of walls}.
% @param    Int Row: rownumber.
% @param    Int Counter: amount of iterations.
% @param    Int Max: maximum number of iterations.
% @param    String: formatted string.
% @returns  Calls the function hlines_counter again with the new String, and
%           therefore forms a recursion with two functions.
hlines_string(Grid, Row, Counter, Max, String) ->
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
% @param    Tuple Grid: the grid {width, height, list of walls}.
% @param    Int Counter: current rownumber.
% @param    Int Height: total height of the grid.
% @returns  Recursively prints the grid until the last row is printed.
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
% @param    int W: Width.
% @param    int H: Height.
% @param    int CountV: current rownumber.
% @param    List AllWalls: list with all walls (Empty on first call).
% @returns  A list of all possible walls in a grid, with duplicates.
get_all_walls(W, H, CountV, AllWalls) ->
    case(CountV) of
        H -> AllWalls;
        _ -> Walls = get_walls_row(W, H, 0, CountV, AllWalls),
             NewCounter = CountV + 1,
             get_all_walls(W, H, NewCounter, Walls)

    end.

% @doc      Gets a list of all walls adjacent to all cells in a row.
% @param    int W: Width.
% @param    int H: Height.
% @param    int CountV: current cellnumber.
% @param    List AllWalls: list with all walls (Empty on first call).
% @returns  A list of all possible walls adjacent to all cells in a row,
%           with duplicates.
get_walls_row(W, H, CountH, Y, AllWalls) ->
    case(CountH) of
        W -> AllWalls;
        _ -> Walls = AllWalls ++ get_cell_walls(CountH, Y),
             NewCounter = CountH + 1,
             get_walls_row(W, H, NewCounter, Y, Walls)
    end.

