
-module(ensy_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, Node_Module} = application:get_env(activator_stub),
    Clock = {ensy_clock, {ensy_clock, start_link, [200]},
        temporary, 2000, worker, [ensy_clock]},
    Activator = {ensy_activator, {ensy_activator, start_link, [Node_Module, {100, 10}]},
        temporary, 2000, worker, [ensy_activator]},
    Nodes_Supervisor = {ensy_nodes_sup, {ensy_nodes_sup, start_link, []},
                        temporary, 2000, supervisor, [ensy_nodes_sup]},
    Children = [Nodes_Supervisor, Activator, Clock],
    Restart_Strategy = {one_for_one, 1, 1},
    {ok, {Restart_Strategy, Children} }.

