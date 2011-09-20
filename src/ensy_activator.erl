
%%% @author Enrico Franchi <enrico.franchi@gmail.com>
%%% @copyright 2011 Enrico Franchi
%%% @doc The activator module uses a callback module where the
%%% functions:
%%% ```
%%% to_spawn(opaque_state(), age()) -> {opaque_state(), [{node_stub, [Init_Args::any()]}]}.
%%% to_activate(opaque_state(), age()) -> {opaque_state(), [pid()]}.
%%% to_destroy(opaque_state(), age()) -> {opaque_state(), [pid()]}.
%%% '''
%%% are defined.
%%% The elements of the list returned by to_spawn are tuples where the first element
%%% indicates the name of the module the newly created nodes use and the second element
%%% is a list of initial argument values. A new node is created for each value in the 
%%% list. Note that if the nodes need a list as an initial argument, the second element
%%% of the tuple will be a list of lists. 
%%% Multiple kind of nodes can be specified. For example:
%%% ```[{kind1, [a, b, c]}, {kind2 [d]}]'''
%%% will create 4 nodes: three of kind `kind', respectively initialized with a, b and c
%%% and one of kind `kind2' initialized with d.
%%% @end
-module(ensy_activator).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-include("time.hrl").

%% @headerfile "time.hrl"

%% @type opaque_state(). The state used by the callback module.
-opaque opaque_state() :: any().

%% @type state() = #state{stub = module(), stub_state = opaque_state()}. `Stub' is the module where the callbacks are defined.
-record(state, {stub, stub_state}).
-type state() :: #state{stub::module(), stub_state::opaque_state()}.

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

-spec start_link({stub_module, module(), [atom()]}) -> Result::any().
% @doc Starts the nodes activator.
% 
% `Module' is an atom representing the module where the callbacks for
% the activator are defined. `Init_Args' are passed to the stub module
% init function.
% Result is a the result of @see gen_server:start_link/4.
% Result is a the result of @see tick/1.
% @end
start_link({stub_module, Module, Init_Args}) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, 
        {Module, Init_Args}, []).

-spec tick(Age::age()) -> ok.
% @doc Send a tick message to the node activator. The activator
% uses the callbacks to determine which nodes to create, which nodes
% to activate and which nodes to destroy. It is an error to call this
% function from other than the clock.
tick(Age) ->
    gen_server:cast(?SERVER, {tick, Age}).

-spec eow(Age::age()) -> {ok, Age::age()}.
% @doc Notifies the activator that the simulation ended. It is an error
% to call this function from other than the clock.
eow(Age) ->
    gen_server:call(?SERVER, {eow, Age}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
% @hidden
-spec init({Module::module(), Init_Args::[atom()]}) -> {ok, state(), non_neg_integer()}.
init({Module, Init_Args}) ->
    Opaque_Stub_Handler = Module:init(Init_Args),
    {ok, #state{stub=Module, stub_state=Opaque_Stub_Handler}, 0}.

% @hidden
-spec handle_cast({atom(), Age::age()}, state()) -> {noreply, state()}.
handle_cast({tick, Age}, #state{stub=Module, stub_state=Opaque_Stub_Handler} = State) ->
    {OSH1, To_Spawn} = Module:to_spawn(Opaque_Stub_Handler, Age),
    {OSH2, To_Activate} = Module:to_activate(OSH1, Age),
    {OSH3, To_Destroy} = Module:to_destroy(OSH2, Age),
    lists:map(fun spawn_node/1, To_Spawn),
    lists:map(fun activate_node/1, To_Activate),
    lists:map(fun destroy_node/1, To_Destroy),
    {noreply, State#state{stub_state=OSH3}}.

% @hidden
handle_call({eow, Age}, _From, State) ->
    {stop, normal, {ok, Age}, State}.

% @hidden
handle_info(_Request, State) ->
    {noreply, State}.

% @hidden
terminate(_Reason, _State) ->
    ok.

% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% @hidden
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

% @private
spawn_node({_Node_Module, Args}) ->
    %ensy_nodes_sup:start_child(Args).
	ok.

% @private
destroy_node(Node_Id) ->
    ensy_nodes_sup:terminate_child(Node_Id).

% @private
activate_node(_Node_Id) ->
    ok.
