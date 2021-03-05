-module(game_server).

-behaviour(gen_server).

-export([start_link/1, handle_call/3, handle_cast/2]).
-export([init/1, move/2, handle_continue/2]).

start_link({W, H, Players}) ->
    gen_server:start_link(game_server, {W, H, Players}, []).

% Abstraction to make a move.
move(Pid, Wall) ->
    gen_server:call(Pid, {move, Wall}).

finished([]) ->
    ok;
finished(Players) -> 
    [H | T] = Players,
    H ! finished,
    finished(T).

% TODO: You need to inform the first player to move.
init({Width, Height, Players}) ->
    Grid = grid:new(Width, Height),
    State = {Grid, Players},
    {ok, State, {continue, move}}.

handle_continue(move, State) ->
    {Grid, Players} = State,
    case grid:filled(Grid) of
        true ->
            finished(Players),
            {stop, normal, State};
        false ->
            hd(Players) ! {move, self(), Grid},
            {noreply, State}
    end.

calculate_score(Wall, Grid) -> 
    {Cell1, Cell2} = Wall,
    cell_score(Cell1, Grid) + cell_score(Cell2, Grid).

cell_score(Cell, Grid) ->
    case grid:is_closed(Cell, Grid) of
        true ->
            1;
        false ->
            0
    end.

decide_next_player(Players) -> 
    lists:reverse(Players).

% TODO: add handle_call for move.
handle_call({move, Wall}, _From, State) ->
    {Grid, Players} = State,
    
    NewGrid = grid:add_wall(Wall, Grid),
    NewPlayers = decide_next_player(Players),

    NewState = {NewGrid, NewPlayers},
    
    Score = calculate_score(Wall, NewGrid),

    {reply, {ok, Score}, NewState, {continue, move}};

% Used for testing.
handle_call(state, _From, State) ->
    {reply, {ok, State}, State};
handle_call({setWalls, Walls}, _From, {{W, H, _}, Players}) ->
    {reply, ok, {{W, H, Walls}, Players}}.

% Required for gen_server behaviour.
% Normally you would implement this too,
% but not required for this assignment.
handle_cast(_, State) ->
    {reply, not_implemented, State}.
