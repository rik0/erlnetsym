
-module(erlnetsym_sup).

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
%    Clock = {erlnetsym_clock, {erlnetsym_clock, start_link, [i]},
%        permanent, 2000, worker, [erlnetsym_clock, erlnetsym_activator]},
    Activator = {erlnetsym_activator, {erlnetsym_activator, start_link, 
            [[fun(_Step, _Max) -> [] end,
                fun(_Step, _Max) -> [] end,
                fun(_Step, _Max) -> [] end]]},
        permanent, 2000, worker, [erlnetsym_activator]},
    %Children = [Activator, Clock],
    Children = [Activator],
    Restart_Strategy = {one_for_one, 5, 5},
    {ok, {Restart_Strategy, Children} }.

