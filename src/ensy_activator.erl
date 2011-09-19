-module(ensy_activator).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-include("time.hrl").

-record(state, {stub, stub_state}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([tick/1, eow/1, start_link/1]).
-export([behaviour_info/1]).


%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------


start_link({stub_module, Module, Init_Args}) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, 
        {Module, Init_Args},
        []).

tick(Age) ->
    gen_server:cast(?SERVER, {tick, Age}).

eow(Age) ->
    gen_server:call(?SERVER, {eow, Age}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init({Module, Init_Args}) ->
    Opaque_Stub_Handler = Module:init(Init_Args),
    {ok, #state{stub=Module, stub_state=Opaque_Stub_Handler}, 0}.

handle_cast({tick, Age}, #state{stub=Module, stub_state=Opaque_Stub_Handler} = State) ->
    {OSH1, To_Spawn} = Module:to_spawn(Opaque_Stub_Handler, Age),
    {OSH2, To_Activate} = Module:to_activate(OSH1, Age),
    {OSH3, To_Destroy} = Module:to_destroy(OSH2, Age),
    lists:map(fun spawn_node/1, To_Spawn),
    lists:map(fun activate_node/1, To_Activate),
    lists:map(fun destroy_node/1, To_Destroy),
    {noreply, State#state{stub_state=OSH3}}.

handle_call({eow, Age}, _From, State) ->
    {stop, normal, {ok, Age}, State}.

handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

behaviour_info(callbacks) ->
       [{init, 1},
        {to_activate, 2},
        {to_destroy, 2},
        {to_spawn, 2}];
behaviour_info(_Other) ->
    undefined.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

spawn_node({_Node_Module, Args}) ->
    ensy_nodes_sup:start_child(Args).

destroy_node(Node_Id) ->
    ensy_nodes_sup:terminate_child(Node_Id).

activate_node(_Node_Id) ->
    ok.
