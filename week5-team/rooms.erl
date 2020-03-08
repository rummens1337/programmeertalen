%%%================================================================================================
% @author Michel Rummens <13108093>
% @author Thomas vos <12829501>
% @copyright 2020 M. Rummens, T vos.
% @version 9000
% @end
%%%================================================================================================

% Please read:
%%% @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
%%% Generate the documentation by executing the following command in erlang shell:
%%% edoc:files(["rooms.erl"], [{dir, doc}]).
%%% As instructed per http://erlang.org/doc/apps/edoc/chapter.html.
%%% @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

-module(rooms).

%%% API
-export([new_grid/2,
        get_wall/3, has_wall/4, add_wall/4, show_vlines/2, show_hlines/2,
        print_grid/1, get_cell_walls/2, get_all_walls/2, get_open_spots/1,
        choose_random_wall/1, build_random_wall/1, get_open_cell_walls/3,
        get_completable_walls/1, get_completable_wall/1, build_wall/1]).

%%%================================================================================================
%%% API
%%%================================================================================================

% @doc
% <p>Description:   Initializes a new grid.</p>
% <p>Param:         Int Width: width of the grid.</p>
% <p>Param:         Int Height: height of the grid.</p>
% <p>Returns:       A tuple with the width, height and empty list for walls of the grid.</p>
% @end
new_grid(Width,Height) ->
   {Width, Height, []}.

% @doc
% <p>Description:   Gets the north, east, south or west wall of a cell.</p>
% <p>Param:         Int X: x-coordinate of the cell.</p>
% <p>Param:         Int Y: y-coordinate of the cell.</p>
% <p>Param:         Atom Dir: direction.</p>
% <p>Returns:       The wall, represented as being between two coordinates -> {{0,0},{-1,0}}.</p>
% @end
get_wall(X,Y,Dir) when Dir == north ->
    {{X,Y-1}, {X,Y}};
get_wall(X,Y,Dir) when Dir == east ->
    {{X,Y}, {X+1,Y}};
get_wall(X,Y,Dir) when Dir == south ->
    {{X,Y}, {X,Y+1}};
get_wall(X,Y,Dir) when Dir == west ->
    {{X-1,Y}, {X,Y}}.

% @doc
% <p>Description:   Checks if the specified wall exists.</p>
% <p>Param:         Int X: x-coordinate of the cell.</p>
% <p>Param:         Int Y: y-coordinate of the cell.</p>
% <p>Param:         Atom Dir: direction.</p>
% <p>Param:         Tuple Grid: the grid {width, height, list of walls}.</p>
% <p>Returns:       Atom true if the specified wall exists, otherwise the atom false.</p>
% @end
has_wall(X, Y, Dir, Grid) ->
    lists:member(get_wall(X,Y,Dir),element(3,Grid)).

% @doc
% <p>Description:   Adds a wall to the grid.</p>
% <p>Param:         Int X: x-coordinate of the cell.</p>
% <p>Param:         Int Y: y-coordinate of the cell.</p>
% <p>Param:         Atom Dir: direction.</p>
% <p>Param:         Tuple Grid: the grid {width, height, list of walls}.</p>
% <p>Returns:       A new grid, with the added wall.</p>
% @end
add_wall(X, Y, Dir, Grid) ->
        {W,H,List} = Grid,
    {W,H,[get_wall(X,Y,Dir) | List]}.

% @doc
% <p>Description:   Calls vlines_counter with the specified argument, 
%                   and returns the final string.</p>
% <p>Param:         Int Row: rownumber.</p>
% <p>Param:         Tuple Grid: the grid {width, height, list of walls}.</p>
% <p>Returns:       Formatted string of the vertical lines of Row.</p>
% @end
show_vlines(Row, Grid) ->
    Max = element(1, Grid),
    StringVlines = vlines_counter(Grid, Row, 0, Max, ""),
    StringVlines.

% @doc
% <p>Description:   Calls hlines_counter with the specified argument, 
%                   and returns the final string.</p>
% <p>Param:         Int Row: rownumber.</p>
% <p>Param:         Tuple Grid: the grid {width, height, list of walls}.</p>
% <p>Returns:       Formatted string of the horizontal lines of Row.</p>
% @end
show_hlines(Row, Grid) ->
    Max = element(1, Grid),
    StringHlines = hlines_counter(Grid, Row, 0, Max, ""),
    StringHlines.

% @doc
% <p>Description:   Calls print_grid/3 with the correct arguments.</p>
% <p>Param:         Tuple Grid: the grid {width, height, list of walls}.</p>
% <p>Returns:       Calls print_grid/3, which prints the grid.</p>
% @end
print_grid(Grid) ->
    Height = element(2, Grid),
    print_grid(Grid, 0, Height).

% @doc
% <p>Description:   Finds all the walls for specific X,Y cell.</p>
% <p>Param:         Int X: x-coordinate of the cell.</p>
% <p>Param:         Int Y: y-coordinate of the cell.</p>
% <p>Returns:       List of all adjacent walls of a cell.</p>
% @end
get_cell_walls(X, Y) ->
    Walls = [get_wall(X, Y, north)] ++ [get_wall(X, Y, east)] ++
               [get_wall(X, Y, south)] ++ [get_wall(X, Y, west)],
    Walls.

% @doc
% <p>Description:   Creates a list with all possible walls in the grid.</p>
% <p>Param:         Int W: width of the grid.</p>
% <p>Param:         Int H: the height of the grid.</p>
% <p>Returns:       A list of all possible walls in a grid, without duplicates.</p>
% @end
get_all_walls(W, H) ->
    Walls = get_all_walls(W, H, 0, []),
    lists:usort(Walls).

% @doc
% <p>Description:   Creates a list of open spots in the grid.</p>
% <p>Param:         Tuple Grid: the grid {width, height, list of walls}.</p>
% <p>Returns:       A list of all open spots (no walls) in a grid.</p>
% @end
get_open_spots(Grid) ->
    get_all_walls(element(1, Grid), element(2, Grid)) -- element(3, Grid).

% @doc
% <p>Description:   Chooses a random open wall from the grid.</p>
% <p>Param:         Tuple Grid: the grid {width, height, list of walls}.</p>
% <p>Returns:       A random open wall.</p>
% @end
choose_random_wall(Grid) ->
    Open = get_open_spots(Grid),
    case(Open) of
        []  -> [];
        _ -> lists:nth(rand:uniform(length(Open)), Open)
    end.

% @doc
% <p>Description:   Builds a random wall in the grid.</p>
% <p>Param:         Tuple Grid: the grid {width, height, list of walls}.</p>
% <p>Returns:       The new grid, with the random wall added.</p>
% @end
build_random_wall(Grid) ->
    {W, H, List} = Grid,
    Wall = choose_random_wall(Grid),
    case(Wall) of
        [] -> no_build;
        _  -> {W,H,lists:usort([Wall | List])}
        end.

% @doc
% <p>Description:   Computes a list of open walls for a given cell (X,Y).</p>
% <p>Param:         Int X: x-coordinate of the cell.</p>
% <p>Param:         Int Y: y-coordinate of the cell.</p>
% <p>Param:         Tuple Grid: the grid {width, height, list of walls}.</p>
% <p>Returns:       A list of open walls for a given cell.</p>
% @end
get_open_cell_walls(X,Y,Grid) ->
    Dirs = [get_wall(X,Y, north),get_wall(X,Y, east),
            get_wall(X,Y, south),get_wall(X,Y, west)],
    lists:sort([Elm || Elm <- Dirs, not(lists:member(Elm, element(3,Grid)))]).

% @doc
% <p>Description:   Computes a list of all the completable walls of a grid.</p>
% <p>Param:         Tuple Grid: the grid {width, height, list of walls}.</p>
% <p>Returns:       A list of completable walls.</p>
% @end
get_completable_walls(Grid) ->
    {W,H,_List} = Grid,
   lists:flatten([get_open_cell_walls(X,Y, Grid)
                    || {X,Y} <- [XY
                    || XY <- lists:flatten([lists:zip(lists:duplicate(W, N), lists:seq(0, H-1))
                    || N <- lists:seq(0, W-1)])], length(get_open_cell_walls(X,Y, Grid)) == 1]).

% @doc
% <p>Description:   Get the first completable wall.</p>
% <p>Param:         Tuple Grid: the grid {width, height, list of walls}.</p>
% <p>Returns:       The first completable wall on success or 
%                   false if no completable walls are found.</p>
% @end
get_completable_wall(Grid) ->
    Walls = get_completable_walls(Grid),
    case(Walls) of
        [] -> false;
        _  -> [H|_T] = Walls, H
    end.

% @doc
% <p>Description:   Build a completable wall, otherwise build random wall.</p>
% <p>Param:         Tuple Grid: the grid {width, height, list of walls}.</p>
% <p>Returns:       Grid with extra element in list of walls.</p>
% @end
build_wall(Grid) ->
    {W,H,List} = Grid,
    Completable = get_completable_wall(Grid),
    case(Completable) of
        false -> build_random_wall(Grid);
        _     -> {W,H,lists:usort([Completable | List])}
        end.

%%%================================================================================================
%%% Internal functions
%%%================================================================================================

% @doc
% <p>Description:   Calls vlines_string with the correct arguments 
%                   based on the state of the counter.</p>
% <p>Param:         Tuple Grid: the grid {width, height, list of walls}.</p>
% <p>Param:         Int Row: rownumber.</p>
% <p>Param:         Int Counter: amount of iterations.</p>
% <p>Param:         Int Max: maximum number of iterations.</p>
% <p>Param:         String: formatted string.</p>
% <p>Returns:       The final formatted string of the vertical lines of a row.</p>
% @end
vlines_counter(Grid, Row, Counter, Max, String) ->
    case(Counter) of
        Max -> vlines_string(Grid, Row, Counter, String);
        _   -> vlines_string(Grid, Row, Counter, Max, String)
    end.

% @doc
% <p>Description:   Formats a vertical wall to a string and adds it to the current
%                   string. If max is specified, the last part of the string will
%                   contain a ~n and the recursion will stop.</p>
% <p>Param:         Tuple Grid: the grid {width, height, list of walls}.</p>
% <p>Param:         Int Row: rownumber.</p>
% <p>Param:         Int Counter: amount of iterations.</p>
% <p>Param:         Int Max: maximum number of iterations.</p>
% <p>Param:         String: formatted string.</p>
% <p>Returns:       The final formatted string of the vertical lines of a row.</p>
% @end
vlines_string(Grid, Row, Counter, String) ->
    NewCounter = Counter - 1,
    Wall = has_wall(NewCounter, Row, east, Grid),

    case(Wall) of
        true  -> NewString = String ++ "|~n",
              NewString;
        false -> NewString = String ++ " ~n",
              NewString
    end.

% @doc
% <p>Description:   Formats a vertical wall to a string and adds it to the current
%                   string. The last part of the string will
%                   contain a ~n and the recursion will stop.</p>
% <p>Param:         Tuple Grid: the grid {width, height, list of walls}.</p>
% <p>Param:         Int Row: rownumber.</p>
% <p>Param:         Int Counter: amount of iterations.</p>
% <p>Param:         Int Max: maximum number of iterations.</p>
% <p>Param:         String: formatted string.</p>
% <p>Returns:       Calls the function vlines_counter again with the new
%                   String, and therefore forms a recursion with two functions.</p>
% @end
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

% @doc
% <p>Description:   Calls hlines_string with the correct arguments based on the state
%                   of the counter.</p>
% <p>Param:         Tuple Grid: the grid {width, height, list of walls}.</p>
% <p>Param:         Int Row: rownumber.</p>
% <p>Param:         Int Counter: amount of iterations.</p>
% <p>Param:         Int Max: maximum number of iterations.</p>
% <p>Param:         String: formatted string.</p>
% <p>Returns:       Calls the function vlines_counter again with the new String,
%                   and therefore forms a recursion with two functions.</p>
% @end
hlines_counter(Grid, Row, Counter, Max, String) ->
    case(Counter) of
        Max -> hlines_string(String);
        _   -> hlines_string(Grid, Row, Counter, Max, String)
    end.

% @doc
% <p>Description:   Places the final plus and ~n on the string with all horizontal walls.</p>
% <p>Param:         String: formatted string.</p>
% <p>Returns:       The final formatted string with horizontal walls.</p>
% @end
hlines_string(String) ->
    NewString = String ++ "+~n",
    NewString.

% @doc
% <p>Description:   Formats a horizontal wall to a string and adds it to the current
%                   string.</p>
% <p>Param:         Tuple Grid: the grid {width, height, list of walls}.</p>
% <p>Param:         Int Row: rownumber.</p>
% <p>Param:         Int Counter: amount of iterations.</p>
% <p>Param:         Int Max: maximum number of iterations.</p>
% <p>Param:         String: formatted string.</p>
% <p>Returns:       Calls the function hlines_counter again with the new String, and
%                   therefore forms a recursion with two functions.</p>
% @end
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

% @doc
% <p>Description:   Prints the full string-formatted grid.</p>
% <p>Param:         Tuple Grid: the grid {width, height, list of walls}.</p>
% <p>Param:         Int Counter: current rownumber.</p>
% <p>Param:         Int Height: total height of the grid.</p>
% <p>Returns:       Recursively prints the grid until the last row is printed.</p>
% @end
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

% @doc
% <p>Description:   Gets a list of all walls.</p>
% <p>Param:         int W: Width.</p>
% <p>Param:         int H: Height.</p>
% <p>Param:         int CountV: current rownumber.</p>
% <p>Param:         List AllWalls: list with all walls (Empty on first call).</p>
% <p>Returns:       A list of all possible walls in a grid, with duplicates.</p>
% @end
get_all_walls(W, H, CountV, AllWalls) ->
    case(CountV) of
        H -> AllWalls;
        _ -> Walls = get_walls_row(W, H, 0, CountV, AllWalls),
             NewCounter = CountV + 1,
             get_all_walls(W, H, NewCounter, Walls)

    end.

% @doc
% <p>Description:   Gets a list of all walls adjacent to all cells in a row.</p>
% <p>Param:         int W: Width.</p>
% <p>Param:         int H: Height.</p>
% <p>Param:         int CountV: current cellnumber.</p>
% <p>Param:         List AllWalls: list with all walls (Empty on first call).</p>
% <p>Returns:       A list of all possible walls adjacent to all cells in a row,
%                   with duplicates.</p>
% @end
get_walls_row(W, H, CountH, Y, AllWalls) ->
    case(CountH) of
        W -> AllWalls;
        _ -> Walls = AllWalls ++ get_cell_walls(CountH, Y),
             NewCounter = CountH + 1,
             get_walls_row(W, H, NewCounter, Y, Walls)
    end.

