-module(erlnetsym_activator).
-behaviour(gen_server).

-include("include/time.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([tick/1, eow/1, start_link/1]).

-define(SERVER, ?MODULE).
-record(state, {activator, spawner, destroyer}).

init([Activator, Spawner, Destroyer]) ->
    {ok, #state{activator=Activator, spawner=Spawner, destroyer=Destroyer}}.

handle_cast({tick, Age}, State) ->
    To_Spawn = (State#state.spawner)(Age),
    To_Activate = (State#state.activator)(Age),
    To_Destroy = (State#state.destroyer)(Age),
    lists:map(fun spawn_node/1, To_Spawn),
    lists:map(fun activate_node/1, To_Activate),
    lists:map(fun destroy_node/1, To_Destroy),
    {noreply, State}.

handle_call({eow, _Age}, _From, State) ->
    %{stop, ok, State}.
    {noreply, State}.

handle_info(Request, State) ->
    io:format("~p~n", [Request]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% API

start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, [{debug, [trace, log, statistics]}]).


tick(Age) ->
    gen_server:cast(erlnetsym_activator, {tick, Age}).

eow(Age) ->
    gen_server:call(erlnetsym_activator, {eow, Age}).

% internal
spawn_node({_Module, _Class, _Init_Args}) ->
    ok.

activate_node({_Node}) ->
    ok.

destroy_node({_Node}) ->
    ok.

