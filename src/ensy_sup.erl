
-module(ensy_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Clock = {ensy_clock, {ensy_clock, start_link, [{iterations, 200}]},
        transient, 2000, worker, [ensy_clock, ensy_activator]},
    Activator = {ensy_activator, {ensy_activator, start_link, []},
        transient, 2000, worker, [ensy_activator]},
    %Children = [Clock, Activator],
    Children = [Clock],
    Restart_Strategy = {one_for_one, 1, 1},
    {ok, {Restart_Strategy, Children} }.

