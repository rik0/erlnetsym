#!/usr/bin/env escript
% vi: filetype=erlang
%% -*- erlang -*-
%% -smp enable -pa ebin -boot start_sasl
%% -smp enable -pa ebin 
%%! -smp enable -pa ebin -boot start_sasl -sasl sasl_error_logger '{file, "/tmp/ensy.log"}'


main(_Args) ->
    code:add_path("ebin"),
    application:start(sasl),
    application:start(ensy, permanent),
    lists:map(fun(App) -> io:format("~p~n", [App]) end, 
        application:which_applications()).