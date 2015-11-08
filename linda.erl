-module(linda).
-compile(export_all).
% -export([run/0]).

% cd('C:/git/linda').

% variables and parameters must begin with a capital letter

% in() ->
% 	ok.

% out(TS, Tuple) ->
% 	TS ++ [Tuple].

% returns our representation for a tuple template
% very naive at the moment, will probably use atoms to represent types
create_template(ElementCount) ->
	{ElementCount}.

% does the tuple conform to the supplied template
template_match(Template, Tuple) ->
	tuple_size(Tuple) == element(1, Template).

fetch_tuple([], _) ->
	false;
fetch_tuple(Bag, Template) ->
	case template_match(Template, hd(Bag)) of
		true ->
			hd(Bag);
		false ->
			fetch_tuple(tl(Bag), Template)
	end.

tuple_space(Bag) ->
	receive
		{out, Tuple} ->
			tuple_space(Bag ++ [Tuple]);
		{in, Template} ->
			% this would need to find and remove the tuple
			tuple_space(Bag)
	end.

run() ->
	% initialise the tuple space with an empty list
	% function must be exported to spawn it as a process
	TupleSpaceProcess = spawn(?MODULE, tuple_space, [[]]),

	% out a couple tuples
	TupleSpaceProcess ! {out, {1,2}},
	TupleSpaceProcess ! {out, {2,1}},

	ok.
	% erlang:display(B).