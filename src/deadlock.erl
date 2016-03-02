-module(deadlock).
-author("Juddling").

-export([status/2]).

status(ClientsAware, ClientsBlocked) ->
  % ensure both lists don't contain duplicates
  false = contains_duplicates(ClientsAware),
  false = contains_duplicates(ClientsBlocked),

  case {length(ClientsAware), length(ClientsBlocked)} of
    {0, 0} ->
      % no one is aware of the TS, so it is garbage
      garbage;
    {0, _} ->
      erlang:error(logic_error);
    {N, N} ->
      % all clients blocked
      deadlock;
    _  ->
      none
  end.

contains_duplicates(List) ->
  erlang:length(List) /= sets:size(sets:from_list(List)).