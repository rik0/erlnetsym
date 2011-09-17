
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
%%%  should_connect(Opaque_State, From::pid(), Neighbours::[pid()])
%%%    ==> {bool(), New_Stub, New_Opaque_State}
%%%
%%%  handle_activate(Opaque_State, Age::age())
%%%    ==> {New_Stub, New_Opaque_State}
%%% 
%%%  handle_drop_connection(Opaque_State, From::pid(), Neighbours::[pid()])
%%%    ==> {New_Stub, New_Opaque_State}
%%%
%%% '''
%%%
%%%
%%% `init' initializes the stub module and return the Opaque_State
%%% that will be passed to all the stub callback functions. These
%%% usually return a tuple containing also a `New_Stub' and a 
%%% `New_Opaque_State'.
%%%
%%% `should_connect' takes an `Opaque_State' and a the pid of the 
%%% node which is requiring the connection and the list of the nodes
%%% currently connected to self(). The first element in the returned
%%% tuple is a boolean which represents the decision of the stub
%%% whether to connect or not.
%%% `New_Stub' is a new stub which will be used to handle future
%%% messages in place of the `Stub'. Usually it is the same than
%%% `Stub'. `New_Opaque_State' is an opaque state that can be
%%% understood by the new stub.
%%%
%%% At the moment, there is no support for initializing the `New_Stub' with some
%%% `Init_Args'.  Consequently, it must be able to work without initialization,
%%% just receiving `New_Stub'.  This feature is however useful to use a strategy
%%% like pattern instead of flags: essentially the former stub should know about
%%% the new one and should ensure that things are going to work.
%%%
%%% `handle_activate' is the callback which exectures whatever it has to be done
%%% when an activate message is sent (e.g., try to connect to the more popular
%%% node).
%%%
%%% `handle_drop_connection' is called when a node severed its connection
%%% with the current node. This is unilateral and cannot be avoided.
%%% However, actions can be performed (e.g., try to negotiate to establish
%%% the connection again.
%%%
%%% Moreover, the usual `handle_cast', `handle_info', `handle_call' are
%%% forwarded to the stub if the message could not be intercepted
%%% by this module (e.g., request_connection and drop_connection
%%% are already intercepted by this base node.
%%% In this case, the callback is only passed the `Opaque_State'.
%%% The return value is expected to follow the same rules of the
%%% corresponding OTP functions. However, whenever a `State' is to
%%% returned a tuple `{New_Stub, New_State}'; `New_Stub' may be diffent
%%% from Stub and should follow the same rules mentioned in the case
%%% of the other callbacks.
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
%% If target would not like the link to be severed, than
%% it can only recast a request_connection and hope it
%% will be accepted.
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
%% `Stub' is an atom representing the module where the callback
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
    case Stub:should_connect(Opaque_State, From, Neighbours) of
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
handle_call(Request, From, 
           #state{stub=Stub,
                 stub_state=Opaque_State} = State) ->
   case Stub:handle_call(Request, From, Opaque_State) of
       {reply, Reply, {New_Stub, New_Opaque_State}} ->
           {reply, Reply,
            State#state{stub=New_Stub, stub_state=New_Opaque_State}};
       {reply,Reply,{New_Stub, New_Opaque_State},hibernate} ->
           {reply, Reply,
            State#state{stub=New_Stub, stub_state=New_Opaque_State},
            hibernate};
       {reply,Reply, {New_Stub, New_Opaque_State},Timeout} -> 
           {reply, Reply,
            State#state{stub=New_Stub, stub_state=New_Opaque_State},
            Timeout};
       {noreply,{New_Stub, New_Opaque_State}}  ->
           {noreply,
            State#state{stub=New_Stub, stub_state=New_Opaque_State}};
       {noreply,{New_Stub, New_Opaque_State},hibernate} ->
           {noreply,
            State#state{stub=New_Stub, stub_state=New_Opaque_State},
            hibernate};
       {noreply, {New_Stub, New_Opaque_State}, Timeout} ->
           {noreply,
            State#state{stub=New_Stub, stub_state=New_Opaque_State},
            Timeout};
       {stop,Reason,Reply,{New_Stub, New_Opaque_State}} ->
           {stop, Reason, Reply,
            State#state{stub=New_Stub, stub_state=New_Opaque_State}};
       {stop,Reason,{New_Stub, New_Opaque_State}} ->
           {stop, Reason,
            State#state{stub=New_Stub, stub_state=New_Opaque_State}}
   end.

handle_cast({activate, Age}, 
    #state{stub=Stub, stub_state=Opaque_State} = State) ->
    {New_Stub, New_Opaque_State} = Stub:handle_activate(Opaque_State, Age),
    {noreply, State#state{stub=New_Stub, stub_state=New_Opaque_State}};
handle_cast({drop_connection, From},
    #state{stub=Stub, 
        stub_state=Opaque_State,
        neighbours=Neighbours} = State) ->
    New_Neighbours = sets:del_element(From, Neighbours),
    {New_Stub, New_Opaque_State} = Stub:handle_drop_connection(
        Opaque_State, From, Neighbours),
    {noreply, State#state{
            stub=New_Stub, 
            stub_state=New_Opaque_State,
            neighbours=New_Neighbours}};
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

handle_info(Info, #state{stub=Stub, stub_state=Opaque_State} = State) ->
    case Stub:handle_info(Info, Opaque_State) of
        {noreply,{New_Stub, New_Opaque_State}} ->
            {noreply,
                State#state{stub=New_Stub, stub_state=New_Opaque_State}};
        {noreply,{New_Stub, New_Opaque_State},hibernate} -> 
            {noreply,
                State#state{stub=New_Stub, stub_state=New_Opaque_State},
                hibernate};
        {noreply,{New_Stub, New_Opaque_State},Timeout} -> 
            {noreply,
                State#state{stub=New_Stub, stub_state=New_Opaque_State},
                Timeout};
        {stop,Reason,{New_Stub, New_Opaque_State}} ->
            {stop, Reason, 
                State#state{stub=New_Stub, stub_state=New_Opaque_State}}
        end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec behaviour_info(atom()) -> 'undefined' | [{atom(), arity()}].

behaviour_info(callbacks) ->
    [
        {init, 1},
        {handle_cast, 2},
        {handle_call, 3},
        {handle_info, 2},
        {handle_activate, 2},
        {should_connect, 2}
    ];
behaviour_info(_Other) ->
    undefined.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

