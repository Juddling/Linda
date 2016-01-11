-module(tuple_space).
-export([out/1, in/1, start/0, stop/0]).
%% gen_server exports
-export([init/1, code_change/3, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).
-behavior(gen_server).

out(Tuple) -> gen_server:call(?MODULE, {out, Tuple}).
%%  tuple_space(Bag ++ [Tuple]).

in(Template) -> gen_server:call(?MODULE, {do, an, in, with, Template}).
%%  % result needs to be sent to the kernel
%%  Result = fetch_tuple(Bag, Template),
%%  io:format('tuple returned was ~w with an in from template: ~w ~n', [Result, Template]),
%%
%%  % if we return a tuple, remove it from the bag, destructive in
%%  if
%%    Result == false ->
%%      tuple_space(Bag),
%%    true ->
%%      tuple_space(lists:delete(Result, Bag)).


%% gen_server functions

start() ->
  %% this will call init()
  %% returns: {ok, <process_id>}
  %% If ServerName={local,Name} the gen_server is registered locally as Name
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:stop(?MODULE).

init([]) ->
  {ok, []}. %% should return the state, use records here


handle_call({out, Tuple}, From, State) ->
  io:format("out request received from '~p' with current state '~p'~n",[From, State]),
  {noreply, State++[Tuple]};
handle_call(Message, From, State) ->
  io:format("Generic call handler: '~p' from '~p' with current state '~p'~n",[Message, From, State]),
  {reply,ok,State}.

terminate(Reason, State) -> ok.

code_change(OldVsn, State, Extra) -> {ok, State}.

handle_info(Info, State) -> ok.

handle_cast(Request, State) -> ok.

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