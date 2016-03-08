-module(deadlock).
-author("Juddling").

-export([status/2]).

status(ClientsAware, ClientsBlocked) ->
  % ensure both lists don't contain duplicates
  false = contains_duplicates(ClientsAware),
  false = contains_duplicates(ClientsBlocked),

  case {length(ClientsAware), length(ClientsBlocked)} of
    {0, 0} ->
      % no one is aware of the TS, potentially garbage
      no_clients;
    {0, _} ->
      erlang:error(logic_error);
    {N, N} ->
      % all clients blocked, potentially a deadlock
      all_clients_blocked;
    _  ->
      none
  end.

contains_duplicates(List) ->
  erlang:length(List) /= sets:size(sets:from_list(List)).