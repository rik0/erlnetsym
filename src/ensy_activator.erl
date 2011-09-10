-module(ensy_activator).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-include("include/time.hrl").

-record(state, {module, init_args}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([tick/1, eow/1, start_link/1]).


%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------


start_link({stub_module, Module, Init_Args}) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [#state{module=Module, init_args=Init_Args}], 
            [{debug, []}]).

tick(Age) ->
    gen_server:cast(?SERVER, {tick, Age}).

eow(Age) ->
    gen_server:call(?SERVER, {eow, Age}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(State) ->
    {ok, State, 0}.

handle_cast({tick, Age}, State) ->
    %To_Spawn = apply(Module, spawner, [Age]),
    %To_Activate = apply(Module, activator, [Age]),
    %To_Destroy = apply(Module, destroyer, [Age]),
    %lists:map(fun spawn_node/1, To_Spawn),
    %lists:map(fun activate_node/1, To_Activate), 
    %lists:map(fun destroy_node/1, To_Destroy),
    {noreply, State}.

handle_call({eow, Age}, _From, State) ->
    io:format("~p~n", [Age]),
    %{stop, normal, State};
    {reply, ok, State};
handle_call(Whatever, From, State) ->
    io:format("Got ~p from ~p~n", [Whatever, From]),
    {reply, ok, State}.

%handle_info(timeout, State) ->
    %ok = apply(Module, init, Init_Args),
    %{noreply, State};
handle_info(Request, State) ->
    io:format("~p~n", [Request]),
    {noreply, State}.

terminate(Reason, State) ->
    io:format("~p, ~p~n", [Reason, State]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

