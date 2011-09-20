-module(ensy_nodes_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         start_child/1,
         terminate_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Opts) ->
    supervisor:start_child(?SERVER, Opts).

terminate_child(Pid) ->
    supervisor:terminate_child(?SERVER, Pid).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Element = {ensy_node, {ensy_node, start_link, []},
               temporary, brutal_kill, worker, []},
    Children = [Element],
    Restart_Strategy = {simple_one_for_one, 0, 1},
    {ok, {Restart_Strategy, Children}}.

