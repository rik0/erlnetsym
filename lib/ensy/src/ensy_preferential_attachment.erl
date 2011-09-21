%%% @author Enrico Franchi <enrico.franchi@gmail.com>
%%% @copyright 2011 Enrico Franchi
%%% @doc Callback module for the activator node.
%%% @end
-module(ensy_preferential_attachment).
-behaviour(ensy_activator).

%%
%% Include files
%%

-include("time.hrl").
%% @headerfile "time.hrl"


%%
%% API Functions
%%

-export([init/1, to_spawn/2, to_destroy/2, to_activate/2]).

-type state() :: {pos_integer(), pos_integer()}.

-spec init({N::pos_integer(), M::pos_integer()}) -> state().
%% @doc Initializes the model. The model starts which `N' nodes. How the nodes are linked is not
%% specified (yet). Different possibilities exist (no links, random).
%% @end
init({N, M}) ->
    {N, M}.

-spec to_spawn(state(), age()) -> {state(), [{atom(), [any()]}]}.
%% @doc N nodes are spawned just at the beginning of the network creation
%% process. After that, a new node (with M contacts) is created at each iteration.
to_spawn({N, _M} = State, #age{current=0}) ->
	To_Spawn = n_isolated_nodes(N, []),
    {State, [{ensy_ba_node, To_Spawn}]};
to_spawn({_N, M} = State, _Age) ->
	Connections = ensy_util:choose_by_degree(M),
    {State, [{ensy_ba_node, Connections}]}.

-spec to_destroy(state(), age()) -> {state(), [pid()]}.
%% @doc In the Barabasi-Albert Preferential Attachment model no nodes are
%% ever destroyed.
%% @end
to_destroy(State, _Age) ->
    {State, []}.

-spec to_activate(state(), age()) -> {state(), [pid()]}.
%% @doc In the Barabasi-Albert Preferential Attachment model no nodes are
%% activated. When they are created they already know to which nodes to connect
%% and just do that.
to_activate(State, _Age) ->
    {State, []}.

%%
%% Local Functions
%%
n_isolated_nodes(0, Lst) -> Lst;
n_isolated_nodes(N, Lst) ->
	n_isolated_nodes(N-1, [[]|Lst]).
