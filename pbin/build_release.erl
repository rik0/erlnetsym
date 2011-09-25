#!/usr/bin/env escript
%%! -pa lib/ensy/ebin

main(_Args) ->
	systools:make_script("releases/erlnetsym", [local]).