-module(tuple_space).
-export([out/1, in/1, dump/0, release/0,
  start/0, stop/0]).
%% gen_server exports
-export([init/1, code_change/3, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).
-behavior(gen_server).

-record(state, {tuples, clients}).

dump() ->
  gen_server:call({global, ?MODULE}, dump).

release() ->
  gen_server:cast({global, ?MODULE}, release).

out(Tuple) ->
  gen_server:cast({global, ?MODULE}, {out, Tuple}).
%%  tuple_space(Bag ++ [Tuple]).

in(Template) ->
  gen_server:call({global, ?MODULE}, {in, Template}, infinity).
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
  gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:stop({global, ?MODULE}).

init([]) ->
  {ok, #state{tuples = [], clients = []}}.

%% handle synchronous requests with handle_call()
handle_call(dump, From, State) ->
  io:format("dump request received from '~p' with current state '~p'~n",[From, State]),
  {reply, ok, State};
handle_call({in, Template}, From, State=#state{clients = Clients}) ->
  {noreply, State#state{clients = Clients ++ [From]}};
handle_call(Message, From, State) ->
  io:format("Generic call handler: '~p' from '~p' with current state '~p'~n",[Message, From, State]),
  {reply, ok, State}.

%% handle asynchronous requests with handle_cast
handle_cast({out, Tuple}, State=#state{tuples = Tuples}) ->
  io:format("tuple: '~p' has been outed~n",[Tuple]),
  {noreply, State#state{tuples = Tuples ++ [Tuple]}};

%% reply to blocked processes
handle_cast(release, State=#state{clients = Clients}) ->
  gen_server:reply(hd(Clients), debug_reply).

terminate(Reason, State) -> ok.

code_change(OldVsn, State, Extra) -> {ok, State}.

handle_info(Info, State) -> ok.

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