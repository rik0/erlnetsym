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
%% @doc Initializes the model.
%%
%% The model starts which `N' nodes. How the nodes are linked is not
%% specified (yet). Different possibilities exist (no links, random).
init({N, M}) ->
    {N, M}.

-spec to_spawn(state(), age()) -> {state(), [{atom(), [any()]}]}.
to_spawn({N, M} = State, #age{current=0}) ->
    {State, []};
to_spawn({N, M} = State, Age) ->
    {State, []}.

-spec to_destroy(state(), age()) -> {state(), [pid()]}.
to_destroy(State, _Age) ->
    {State, []}.

-spec to_activate(state(), age()) -> {state(), [pid()]}.
to_activate(State, Age) ->
    {State, []}.

%%
%% Local Functions
%%

