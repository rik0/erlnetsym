-module(stub_model).
-behaviour(ensy_activator).

-export([init/1, to_spawn/2, to_destroy/2, to_activate/2]).

init(_Whatever) ->
    ok.

to_spawn(ok, _Age) ->
    {ok, []}.

to_destroy(ok, _Age) ->
    {ok, []}.

to_activate(ok, Age) ->
    {ok, []}.
