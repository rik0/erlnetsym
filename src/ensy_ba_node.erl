%% Author: enrico
%% Created: Sep 20, 2011
%% Description: TODO: Add description to ensy_ba_node
-module(ensy_ba_node).
-behaviour(ensy_node).

-define(NODE, ?MODULE).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([init/1, handle_activate/2,
		 handle_call/3, handle_cast/2,
		 handle_info/2, should_connect/3,
		 handle_drop_connection/3]).

%%
%% API Functions
%%

-spec init([pid()]) -> {ok, [pid()], 0}.
%% @doc Each node in the BA model is initialized with a list of nodes it should
%% connect to. A timeout is set and when triggered, the connections are performed.
%% BA nodes should never receive any other kind of message.
%% @end
init([Connections]) ->
	{ok, Connections, 0}.

%% @hidden
handle_activate(State, Age) ->
	error_logger:warning_report([{message, activate}, 
								 {state, State}, 
								 {pid, self()}, 
								 {age, Age}]),
	{?NODE, State}.

%% @hidden
should_connect(State, _From, _Neighbours) ->
	{true, ?NODE, State}.

%% @hidden
handle_cast(Msg, State) ->
	error_logger:warning_report([{message, Msg}, 
								 {state, State}, 
								 {pid, self()}]),
	{noreply, {?NODE, State}}.

%% @hidden
handle_call(Msg, From, State) ->
	error_logger:warning_report([{message, Msg}, 
								 {from, From},
								 {state, State}, 
								 {pid, self()}]),
	{noreply, {?NODE, State}}.

%% @hidden
handle_drop_connection(State, From, _Neighbours) ->
	error_logger:warning_report([{message, drop_connection},
								 {from, From},
								 {state, State},
								 {pid, self()}]),
	{?NODE, State}.

%% @hidden
handle_info(timeout, []) ->
	{noreply, []};
handle_info(timeout, [N|Ns]) ->
	ensy_node:request_connection(N),
	{noreply, Ns, 1};
handle_info(Msg, State) ->
	error_logger:warning_report([{message, Msg}, 
								 {state, State}, 
								 {pid, self()}]),
	{noreply, {?NODE, State}}.
%%
%% Local Functions
%%

