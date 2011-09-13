-module(ensy_node).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0,
         request_connection/1,
         activate/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([behaviour_info/1]).

-define(request_connection_message(From, To), {request_connection, From, To}).

-record(state, {stub, neighbours, stub_state}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

request_connection(Target) ->
    Msg = ?request_connection_message(self(), Target),
    case gen_server:call(Target, Msg) of
        ok -> ok;
        _ -> fail
    end.

activate(Node, Age) ->
    gen_server:cast(Node, {activate, Age}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init({Stub, Init_Args}) ->
    Opaque_State = Stub:init(Init_Args),
    {ok, #state{stub=Stub, neighbours=sets:new(), stub_state=Opaque_State}}.

handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

handle_cast({activate, Age}, #state{stub=Stub, stub_state=Opaque_State} = State) ->
    {New_Stub, New_Opaque_State} = Stub:handle_activate(Opaque_State, Age),
    {noreply, State#state{stub=New_Stub, stub_state=New_Opaque_State}};
handle_cast(Msg, #state{stub=Stub, stub_state=Opaque_State} = State) ->
    case Stub:handle_cast(Msg, Opaque_State) of
        {noreply, {New_Stub, New_Opaque_State}} ->
            {noreply, State#state{stub=New_Stub, stub_state=New_Opaque_State}};
        {noreply, {New_Stub, New_Opaque_State, Timeout_Or_Hibernate}} ->
            {noreply, State#state{stub=New_Stub, stub_state=New_Opaque_State},
             Timeout_Or_Hibernate};
        {stop, Reason, {New_Stub, New_Opaque_State}} ->
            {stop, Reason, State#state{stub=New_Stub, stub_state=New_Opaque_State}}
    end.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


behaviour_info(callbacks) ->
       [{init, 1},
        {handle_cast, 2}
        ];
behaviour_info(_Other) ->
    undefined.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

