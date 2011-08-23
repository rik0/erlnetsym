-module(erlnetsym_clock).

-export([spawn_link/1]).


spawn_link(Args) ->
    io:format("DFDFD~n"),
    io:format("~p~n", [Args]),
    [Max_Steps] = Args,
    spawn_link(?MODULE, run, [0, Max_Steps]).

run(Step, Max_Steps) when Step < Max_Steps ->
    erlnetsym_activator:tick(Step, Max_Steps),
    Next_Step = Step + 1,
    run(Next_Step, Max_Steps);
run(Step, _Max_Steps) ->
    erlnetsym_activator:eow(Step).



