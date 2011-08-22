-module(erlnetsym_clock).

-export([init/1]).


init(Max_Steps) ->
    run(0, Max_Steps).

run(Step, Max_Steps) when Step < Max_Steps ->
    erlnetsym_activator:tick(Step, Max_Steps),
    Next_Step = Step + 1,
    run(Next_Step, Max_Steps);
run(Step, _Max_Steps) ->
    ok = erlnetsym_activator:eow(Step).



