-module(linda).
-compile(export_all).
% -export([run/0]).

% cd('C:/git/linda').
% variables and parameters must begin with a capital letter



run() ->
	% initialise the tuple space with an empty list
	% function must be exported to spawn it as a process
	TupleSpaceProcess = spawn(?MODULE, tuple_space, [[]]),

	% out a couple tuples
	TupleSpaceProcess ! {out, {1,2}},
	TupleSpaceProcess ! {out, {2,1}},
	TupleSpaceProcess ! {in, {integer}},
	TupleSpaceProcess ! {in, {integer, 2}},
	TupleSpaceProcess ! {in, {integer, 2}},
	TupleSpaceProcess ! {in, {integer, integer}},
	TupleSpaceProcess ! {in, {integer, integer}},
	timer:sleep(500),

	ok.
	% erlang:display(B).