-module(ensy_clock).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-include("time.hrl").
%% @headerfile "time.hrl"

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

-spec start_link(pos_integer()) -> any().
%% @doc Starts the clock. 
%%
%% `Iterations' is the number of iterations the simulation is meant to do.
%% After that, the clock sends and `eow' message to the activator and 
%% everything stops.
start_link(Iterations) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Iterations, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%% @hidden
-spec init({iterations, Max_Ticks::pos_integer()}) -> {ok, age(), 0}.
init({iterations, Max_Ticks}) ->
	Age = #age{current=0, max=Max_Ticks},
    {ok, Age, 0}.

%% @hidden
handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

%% @hidden
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @hidden
%% @doc We are using timeouts to wake up to send the `tick' 
%% messages to the activator.
%% @end
handle_info(timeout, #age{current = Current, max=Max} = Time) ->
    case Current of
        Max -> 
            {ok, Time} = ensy_activator:eow(Time),
            {stop, normal, Time};
        _ ->
            ensy_activator:tick(Time),
            {noreply, Time#age{current = Current + 1}, 1}
    end;
handle_info(Info, State) ->
    error_logger:info_report({unknown_data, Info}),
    {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

