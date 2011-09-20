
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
	{ok, Activator_Setup_Args} = application:get_env(activator_stub_init),
	{ok, Simulation_Steps} = application:get_env(simulation_steps),
    Clock = {ensy_clock, {ensy_clock, start_link, [Simulation_Steps]},
        temporary, 2000, worker, [ensy_clock]},
    Activator = {ensy_activator, {ensy_activator, start_link, [Node_Module, Activator_Setup_Args]},
        temporary, 2000, worker, [ensy_activator]},
    Nodes_Supervisor = {ensy_nodes_sup, {ensy_nodes_sup, start_link, []},
                        temporary, 2000, supervisor, [ensy_nodes_sup]},
    Children = [Nodes_Supervisor, Activator, Clock],
    Restart_Strategy = {one_for_one, 1, 1},
    {ok, {Restart_Strategy, Children} }.

