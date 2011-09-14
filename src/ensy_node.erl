
%%% @author Enrico Franchi <enrico.franchi@gmail.com>
%%% @copyright 2011 Enrico Franchi
%%% @doc To implement an actual node in the network the
%%% user provides some callback functions.
%%%
%%% The user module supplies the functions:
%%% 
%%% ```
%%%  init(Init_Args) 
%%%    ==> Opaque_State
%%%
%%%  should_connect(Opaque_State, From)
%%%    ==> {bool(), New_Stub, New_Opaque_State}
%%%
%%%  should_drop_connection(Opaque_State, From)
%%%    ==> {bool(), New_Stub, New_Opaque_State}
%%%  
%%%  handle_activate(Opaque_State, Age)
%%%    ==> {New_Stub, New_Opaque_State}
%%% '''
%%% 
%%% 
%%% 
%%%  baz.
%%% @end
%%% -----------------------------------------------------

-module(ensy_node).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0,
        request_connection/1,
        drop_connection/1,
        activate/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).
-export([behaviour_info/1]).


-record(state, {stub, neighbours, stub_state}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% @doc Starts a generic node.
%%
%% @spec start_link() -> {ok, Pid}
%%                    | {error, {already_started, Pid}}
%%                    | {error, Reason}
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @spec request_connection(pid()) -> ok | fail
%% @doc Send a connection request to `Target'.
request_connection(Target) ->
    gen_server:call(Target, request_connection).


%% @spec drop_connection(pid()) -> ok
%% @doc Informs `Target' that the link should be severed.
drop_connection(Target) ->
    gen_server:cast(Target, drop_connection).

%% @spec activate(Node::pid(), Age::age()) -> ok
%% @doc Send an activation message to `Node'. This is usually 
%% sent by the activator process.
activate(Node, Age) ->
    gen_server:cast(Node, {activate, Age}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%% @spec init(Args::{Stub::module(), Init_Args}) -> {ok, State}
%% @doc Initializes the current node.
%%
%% `Stub' is an atom representing the functions where the callback
%% functions are defined. `Init_Args' are passed to an initialization
%% function defined in that module as `Stub:init(Init_Args)'.
%% @end
init({Stub, Init_Args}) ->
    Opaque_State = Stub:init(Init_Args),
    {ok, #state{stub=Stub, 
            neighbours=sets:new(), 
            stub_state=Opaque_State}}.

handle_call(request_connection, From,
    #state{stub=Stub,
        stub_state=Opaque_State,
        neighbours=Neighbours} = State) ->
    case Stub:should_connect(Opaque_State, From) of
        {true, New_Stub, New_Opaque_State} ->
            {reply, ok,
                State#state{
                    stub=New_Stub,
                    stub_state=New_Opaque_State,
                    neighbours=sets:add_element(From, Neighbours)}};
        {false, New_Stub, New_Opaque_State} ->
            {reply, fail,
                State#state{
                    stub=New_Stub,
                    stub_state=New_Opaque_State,
                    neighbours=Neighbours}}
    end;
handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

handle_cast({activate, Age}, 
    #state{stub=Stub, stub_state=Opaque_State} = State) ->
    {New_Stub, New_Opaque_State} = Stub:handle_activate(Opaque_State, Age),
    {noreply, State#state{stub=New_Stub, stub_state=New_Opaque_State}};
handle_cast(Msg, #state{stub=Stub, stub_state=Opaque_State} = State) ->
    case Stub:handle_cast(Msg, Opaque_State) of
        {noreply, {New_Stub, New_Opaque_State}} ->
            {noreply, State#state{
                    stub=New_Stub, 
                    stub_state=New_Opaque_State}};
        {noreply, {New_Stub, New_Opaque_State, Timeout_Or_Hibernate}} ->
            {noreply, State#state{
                    stub=New_Stub, 
                    stub_state=New_Opaque_State},
                Timeout_Or_Hibernate};
        {stop, Reason, {New_Stub, New_Opaque_State}} ->
            {stop, Reason, State#state{
                    stub=New_Stub, 
                    stub_state=New_Opaque_State}}
    end.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec behaviour_info(atom()) -> 'undefined' | [{atom(), arity()}].

behaviour_info(callbacks) ->
    [{init, 1},
        {handle_cast, 2}
    ];
behaviour_info(_Other) ->
    undefined.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

