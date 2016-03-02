-module(template).
-export([fetch_tuple/2, template_match/2]).

%% PRIVATE FUNCTIONS

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
% could add a special atom 'any' which would match any type
template_match_element(integer, Element) ->
  is_integer(Element);
template_match_element(float, Element) ->
  is_float(Element);
template_match_element(boolean, Element) ->
  is_boolean(Element);
template_match_element(Value, Element) ->
  Value == Element.

% return a tuple from a bag that matches the supplied template, if no match is found
% return false
fetch_tuple([], _) ->
  false;
fetch_tuple(Bag, Template) ->
  case template_match(Template, hd(Bag)) of
    true ->
      hd(Bag);
    false ->
      fetch_tuple(tl(Bag), Template)
  end.