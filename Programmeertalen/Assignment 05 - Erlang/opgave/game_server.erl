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

handle_continue(move, {Grid, Players}) ->
    %io:fwrite("AAAA"),
    io:fwrite("~w",[length(grid:get_open_spots(Grid))]),
    case length(grid:get_open_spots(Grid)) == 0 of
        true ->
            finished(Players),
            {noreply, {Grid, Players}};
        false ->
            decide_next_player(Players) ! {move, self(), Grid},
            {noreply, {Grid, Players}}
    end.

calculate_score() -> 1.

decide_next_player(Players) -> hd(Players).

% TODO: add handle_call for move.
handle_call({move, Wall}, _From, State) ->
    {Grid, Players} = State,
    Score = calculate_score(),

    NewGrid = grid:add_wall(Wall, Grid),
    NewState = {NewGrid, Players},
    
    {reply, {ok, Score}, NewState, {continue, move}};

    % Process move .
    % Score berekenen .
    % Score opslaan .
    % Volgende speler bepalen .
    % Continue !!
    % ---
    % Continue
    %   Nieuwe ronde?
    %       JA: Stuur move signaal
    %       NEE: Game finished

    % Bepalen wie is er aan de beurt?
    % 

    % {reply, {ok, Score}, State};

    % 1. Muur aanmaken
    % 2. Zijn er nog plekken over?
    %   - Ja : OK
    
    %   - Nee: Finished

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
