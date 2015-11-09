-module(linda).
-compile(export_all).
% -export([run/0]).

% cd('C:/git/linda').
% variables and parameters must begin with a capital letter

% does the tuple conform to the supplied template
template_match(Template, Tuple) ->
	template_match(Template, Tuple, 1).
template_match(Template, Tuple, _) when tuple_size(Template) /= tuple_size(Tuple) ->
	false;
% if the index exceeds the tuple size, then all elements have matched
template_match(_, Tuple, Index) when Index > tuple_size(Tuple) ->
	true;
template_match(Template, Tuple, Index) ->
	case template_match_element(element(Index, Template), element(Index, Tuple)) of
		true ->
			template_match(Template, Tuple, Index + 1);
		false ->
			false
	end.

% does the element in the template match the element in the actual tuple
% we're using atoms to represent types
template_match_element(integer, Element) ->
	is_integer(Element);
template_match_element(float, Element) ->
	is_float(Element);
template_match_element(boolean, Element) ->
	is_boolean(Element);
template_match_element(Value, Element) ->
	Value == Element.

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
			% result needs to be sent to the kernel
			io:format('tuple returned was ~w with an in from template: ~w ~n', [fetch_tuple(Bag, Template), Template]),
			tuple_space(Bag)
	end.

run() ->
	% initialise the tuple space with an empty list
	% function must be exported to spawn it as a process
	TupleSpaceProcess = spawn(?MODULE, tuple_space, [[]]),

	% out a couple tuples
	TupleSpaceProcess ! {out, {1,2}},
	TupleSpaceProcess ! {out, {2,1}},
	TupleSpaceProcess ! {in, {integer}},
	TupleSpaceProcess ! {in, {integer, 2}},
	TupleSpaceProcess ! {in, {integer, integer}},
	timer:sleep(500),

	ok.
	% erlang:display(B).