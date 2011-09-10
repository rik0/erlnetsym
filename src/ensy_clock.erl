-module(ensy_clock).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-include("include/time.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Max_Ticks) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Max_Ticks], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Max_Ticks]) ->
    {ok, #age{current=0, max=Max_Ticks}, 0}.

handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, #age{current = Current, max=Max} = Time) ->
    case Current of
        Max -> 
            ensy_activator:eow(Time),
            {stop, normal, Time};
        _ ->
            ensy_activator:tick(Time),
            {noreply, Time#age{current = Current + 1}, 1}
    end;
handle_info(Info, State) ->
    error_logger:info_report({unknown_data, Info}),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

