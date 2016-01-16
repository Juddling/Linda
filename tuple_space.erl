-module(tuple_space).
-export([out/1, in/1, dump/0, release/0,
  start/0, stop/0]).
%% gen_server exports
-export([init/1, code_change/3, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).
-behavior(gen_server).

-record(state, {tuples, in_requests}).
-record(in_request, {client, template}).

dump() ->
  gen_server:call({global, ?MODULE}, dump).

release() ->
  gen_server:cast({global, ?MODULE}, release).

out(Tuple) ->
  gen_server:cast({global, ?MODULE}, {out, Tuple}).

in(Template) ->
  gen_server:call({global, ?MODULE}, {in, Template}, infinity).

%% gen_server functions
start() ->
  %% this will call init()
  %% returns: {ok, <process_id>}
  %% server's name is registered globally
  gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:stop({global, ?MODULE}).

init([]) ->
  {ok, #state{tuples = [], in_requests = []}}.

%% checks if the newly added tuple matches the template of any blocked clients
%% if it does, then we reply to them, and unblock
%%
%% behavioural issue: if two clients match the same tuple,
%% do they both receive it, for a non-destructive in()?
reply_blocked_clients([InReq|Tail], NewTuple) ->
  Result = template:template_match(InReq#in_request.template, NewTuple),

  if
    Result /= false ->
      gen_server:reply(InReq#in_request.client, NewTuple);
    true -> ok
  end,

  %% recurse through the rest of the in requests
  reply_blocked_clients(Tail, NewTuple);
reply_blocked_clients([], _) ->
  ok.

%% handle synchronous requests with handle_call()
%% DEBUG function to show current stored tuples
handle_call(dump, _From, State=#state{tuples = Tuples}) ->
  {reply, Tuples, State};
handle_call({in, Template}, From, State=#state{in_requests = InReqs, tuples = Tuples}) ->
  Result = template:fetch_tuple(Tuples, Template),
  if
    %% if a matching tuple isn't found, store this in request
    Result == false ->
      InRequest = #in_request{client = From, template = Template},
      {noreply, State#state{in_requests = InReqs ++ [InRequest]}};
    true ->
      {reply, Result, State}
  end;
handle_call(Message, From, State) ->
  io:format("Generic call handler: '~p' from '~p' with current state '~p'~n",[Message, From, State]),
  {reply, ok, State}.

%% handle asynchronous requests with handle_cast
handle_cast({out, Tuple}, State=#state{tuples = Tuples, in_requests = InRequests}) ->
  io:format("tuple: '~p' has been outed~n",[Tuple]),
  reply_blocked_clients(InRequests, Tuple),
  %% maybe shouldn't append to the end of the list for efficiency
  {noreply, State#state{tuples = Tuples ++ [Tuple]}};

%% DEBUG function to release blocked processes
%% reply to the first blocked process, and then remove
%% the client from the state
handle_cast(release, State=#state{in_requests = []}) ->
  {noreply, State};
handle_cast(release, State=#state{in_requests = [Head|Tail]}) ->
  gen_server:reply(Head#in_request.client, debug_reply),
  {noreply, State#state{in_requests = Tail}}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle_info(_Info, _State) -> ok.