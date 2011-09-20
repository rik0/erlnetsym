% vi: filetype=erlang
%% -*- erlang -*-

-record(age, {current :: pos_integer(), 
		      max :: pos_integer()}).
-type age() :: #age{current :: pos_integer(), 
					max :: pos_integer()}.
