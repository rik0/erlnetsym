
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
    Node_Module = foo,
    Clock = {ensy_clock, {ensy_clock, start_link, [{iterations, 200}]},
        temporary, 2000, worker, [ensy_clock]},
    Activator = {ensy_activator, {ensy_activator, start_link, [{stub_module, Node_Module, []}]},
        temporary, 2000, worker, [ensy_activator]},
    Nodes_Supervisor = {ensy_nodes_sup, {ensy_nodes_sup, start_link, [{stub_module, Node_Module}]},
                        temporary, 2000, supervisor, [ensy_nodes_sup]},
    Children = [Clock, Activator, Nodes_Supervisor],
    Restart_Strategy = {one_for_one, 1, 1},
    {ok, {Restart_Strategy, Children} }.

