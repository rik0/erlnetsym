-module(ensy_nodes_sup).

-behaviour(supervisor).

%% API
-export([start_link/1,
         start_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% ===================================================================
%% API functions
%% ===================================================================

start_link({stub_module, Node_Module}) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Node_Module]).

start_child(Opts) ->
    supervisor:start_child(?SERVER, Opts).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Node_Module]) ->
    Element = {Node_Module, {Node_Module, start_link, []},
               temporary, brutal_kill, worker, [Node_Module]},
    Children = [Element],
    Restart_Strategy = {simple_one_for_one, 0, 1},
    {ok, {Restart_Strategy, Children}}.

