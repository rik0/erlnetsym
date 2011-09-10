-module(erlnetsym_generic).
-define(NETDB, netdb).

-export([force_link/2, require_link/2, process_cast/3, process_call/4]).



force_link(Source, Target) ->
    gen_server:cast(Target, {must_link, Source}).

require_link(Source, Target) ->
    gen_server:call(Target, {should_link, Source}).

process_cast(_Module, {must_link, Source}, State) ->
    gen_server:cast(?NETDB, {link, Source, self()}),
    {noreply, State};
process_cast(_Module, {must_unlink, Source}, State) ->
    gen_server:cast(?NETDB, {unlink, Source, self()}),
    {noreply, State};
process_cast(Module, Message, State) ->
    apply(Module, process_cast, [Message, State]).

process_call(Module, {should_link, Source}, _From = Source, State) ->
    case apply(Module, should_link, [Source]) of
        true ->
            gen_server:cast(?NETDB, {link, Source, self()}),
            {reply, ok, State};
        false ->
            {reply, no, State}
    end;
process_call(_Module, {should_link, Source}, From, State) ->
    error_logger:info_report({should_link, Source, '\=', From}),
    {noreply, State};
process_call(Module, {should_unlink, Source}, _From = Source, State) ->
    case apply(Module, should_unlink, [Source]) of
        true ->
            gen_server:cast(?NETDB, {unlink, Source, self()}),
            {reply, ok, State};
        false ->
            {reply, no, State}
    end;
process_call(_Module, {should_unlink, Source}, From, State) ->
    error_logger:info_report({should_unlink, Source, '\=', From}),
    {noreply, State};
process_call(Module, Message, From, State) ->
    apply(Module, process_call, [Message, From, State]).




