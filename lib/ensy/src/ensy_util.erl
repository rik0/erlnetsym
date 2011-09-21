%% Author: enrico
%% Created: Sep 20, 2011
-module(ensy_util).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([choose_by_degree/1]).

%%
%% API Functions
%%

-spec choose_by_degree(pos_integer()) -> [pid()].
%% @doc Chose `M' nodes with probability proportional to their degree.
%% @end
choose_by_degree(_M) ->
	[].


%%
%% Local Functions
%%

