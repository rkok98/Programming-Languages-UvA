% Author	René Kok <13671146>
% Study 	Doorstroomminor Software Engineering UvA
%
% Implements a game of 'dots and boxes'.

-module(grid).
-export([new/2, get_wall/3, add_wall/2, has_wall/2, show_hlines/2, show_vlines/2, 
	     get_cell_walls/2, get_all_walls/2, get_open_spots/1, choose_random_wall/1, 
		 is_closed/2, filled/1, print/1]).

% Initialize a new grid.
new(Width, Height) -> {Width, Height, []}.

% Returns the wall of a cell based on the given direction.
get_wall(X, Y, north) ->
	{{X, Y - 1}, {X, Y}};
get_wall(X, Y, east) ->
	{{X, Y}, {X + 1, Y}};
get_wall(X, Y, south) ->
	{{X, Y}, {X, Y + 1}};
get_wall(X, Y, west) ->
	{{X - 1, Y}, {X, Y}}.

% Adds a wall to the given grid.
add_wall(Wall, Grid) ->
	{Width, Height, Walls} = Grid,
	
	case has_wall(Wall, Grid) of
		false -> {Width, Height, lists:sort([Wall | Walls])};
		true -> Grid
	end.

% Checks if a grid has the given wall.
has_wall(Wall, Grid) -> 
	{_, _, Walls} = Grid,
	lists:member(Wall, Walls).

% Returns the walls of a given cell.
get_cell_walls(X,Y) ->
    [get_wall(X, Y, Dir) || Dir <- [north, east, south, west]].

% Returns a list of walls based on given width and height.
get_all_walls(Width, Height) ->
	Walls = lists:merge([ get_cell_walls(X, Y) || X <- lists:seq(0, Width - 1), Y <- lists:seq(0, Height - 1)]),
	lists:usort(Walls).

% Returns a list of open positions based on given grid.
get_open_spots(Grid) -> 
	{Width, Height, Walls} = Grid,
	Cells = get_all_walls(Width, Height),
	Cells -- Walls.

% Chooses a open position to place a wall.
choose_random_wall(Grid) ->
	Open = get_open_spots(Grid),
	case Open /= [] of
		true ->
			lists:nth(rand:uniform(length(Open)), Open);
		false ->
			[]
	end.

% Validates if an cell is closed (all fenced walls are drawn).
is_closed(Cell, Grid) ->
	{X, Y} = Cell,
	{_, _, Walls} = Grid,
	FreeWalls = get_cell_walls(X,Y) -- Walls,
	FreeWalls == [].

% Checks if an grid is filled.
filled(Grid) ->
	get_open_spots(Grid) == [].

% Prints this grid in a structured format
% using the show_Xlines functions.
print(Grid) ->
	{_, H, _} = Grid,
	lists:map(fun(Row) ->
		io:fwrite(show_hlines(Row, Grid)),

		case Row < H of
			true ->
				io:fwrite(show_vlines(Row, Grid));
			false ->
				ok
		end
	end, lists:seq(0, H)),
	io:fwrite("~n"),
	ok.

% Returns the horizontal walls line of a grid.
show_hlines(Row, Grid) -> 
	{Width, _, _} = Grid,
	R = ["+" ++ draw_hline({{X, Row - 1}, {X, Row}}, Grid) || X <- lists:seq(0, Width)],

	string:strip(lists:flatten(R), right) ++ "~n".

% Returns the right visualization of a horizontal wall.
draw_hline(Wall, Grid) ->
	case has_wall(Wall, Grid) of
		true ->
			"--";
		false ->
			"  "
	end.

% Returns the vertical walls line of a grid.
show_vlines(Row, Grid) ->
	{Width, _, _} = Grid,
	R = [draw_vline({{X - 1, Row}, {X, Row}}, Grid) ++ "  " || X <- lists:seq(0, Width)],
	A = lists:flatten(R),

	{L, _} = lists:split(length(A) - 2, A),
	L ++ "~n".

% Returns the right visualization of a vertical wall.
draw_vline(Wall, Grid) ->
	case has_wall(Wall, Grid) of
		true ->
			"|";
		false ->
			" "
	end.
