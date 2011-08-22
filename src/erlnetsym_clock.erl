-module(erlnetsym_clock).

-export([init/1]).


init(Max_Steps) ->
    run(0, Max_Steps).

run(Step, Max_Steps) when Step < Max_Steps ->
    erlnetsym_activator:tick(Step),
    Next_Step = Step + 1,
    run(Next_Step, Max_Steps);
run(_Step, _Max_Steps) ->
    erlnetsym_activator.



