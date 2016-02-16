-module(tuple_space).
-export([out/2, in/2, rd/2, dump/1, release/1, size/1,
  start/1, stop/1]).
% gen_server exports
-export([init/1, code_change/3, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).
-behavior(gen_server).

-record(state, {tuples, in_requests}).
-record(in_request, {client, template, destructive = true}).

dump(Name) ->
  gen_server:call({global, Name}, dump).

release(Name) ->
  gen_server:cast({global, Name}, release).

size(Name) ->
  gen_server:call({global, Name}, size).

%% @doc add a tuple to the space
out(Name, Tuple) when is_tuple(Tuple) ->
  gen_server:cast({global, Name}, {out, Tuple}).

in(Name, Template) when is_tuple(Template) ->
  gen_server:call({global, Name}, {in, Template}, infinity).

rd(Name, Template) when is_tuple(Template) ->
  gen_server:call({global, Name}, {read, Template}, infinity).

% gen_server functions
start(Name) ->
  % this will call init()
  % returns: {ok, <process_id>}
  % server's name is registered globally
  gen_server:start_link({global, Name}, ?MODULE, [], []).

stop(Name) ->
  gen_server:stop({global, Name}).

init([]) ->
  {ok, #state{tuples = [], in_requests = []}}.

%% @doc checks if the newly added tuple matches the template of any blocked clients
%% if it does, then we reply to them, and unblock
%%
%% behavioural issue: if two clients match the same tuple,
%% do they both receive it, for a non-destructive in()?
reply_blocked_clients([InReq | Tail], NewTuple) ->
  Result = template:template_match(InReq#in_request.template, NewTuple),

  case Result of
    false ->
      % recurse through the rest of the in requests
      reply_blocked_clients(Tail, NewTuple);
    _ ->
      gen_server:reply(InReq#in_request.client, NewTuple),
      {match, InReq}
  end;
reply_blocked_clients([], _) ->
  {nomatch}.

handle_call(size, _From, State = #state{tuples = Tuples}) ->
  {reply, length(Tuples), State};
%% @doc DEBUG function to show current stored tuples
handle_call(dump, _From, State = #state{tuples = Tuples}) ->
  {reply, Tuples, State};
handle_call({read, Template}, From, State = #state{in_requests = InReqs, tuples = Tuples}) ->
  Result = template:fetch_tuple(Tuples, Template),
  if
    Result == false ->
      % if a matching tuple isn't found, store this in request
      InRequest = #in_request{client = From, template = Template, destructive = false},
      {noreply, State#state{in_requests = InReqs ++ [InRequest]}};
    true ->
      % don't touch the state, non-destructive
      {reply, Result, State}
  end;
handle_call({in, Template}, From, State = #state{in_requests = InReqs, tuples = Tuples}) ->
  Result = template:fetch_tuple(Tuples, Template),
  if
    Result == false ->
      % if a matching tuple isn't found, store this in request
      InRequest = #in_request{client = From, template = Template, destructive = true},
      {noreply, State#state{in_requests = InReqs ++ [InRequest]}};
    true ->
      % remove the tuple from the space
      {reply, Result, State#state{tuples = lists:delete(Result, Tuples)}}
  end;
handle_call(Message, From, State) ->
  io:format("Generic call handler: '~p' from '~p' with current state '~p'~n", [Message, From, State]),
  {reply, ok, State}.

handle_cast({out, Tuple}, State = #state{tuples = Tuples, in_requests = InRequests}) ->
  %% io:format("tuple: '~p' has been outed~n", [Tuple]),

  case reply_blocked_clients(InRequests, Tuple) of
    {match, InRequest} ->
      % this tuple matched a template that had already been requested
      % remove the in request and remove the tuple if the request was destructive

      NewTuples = case InRequest#in_request.destructive of
                    true  -> Tuples;
                    _     -> Tuples ++ [Tuple]
                  end,

      {noreply, State#state{in_requests = lists:delete(InRequest, InRequests), tuples = NewTuples}};
    {nomatch} ->
      % maybe shouldn't append to the end of the list for efficiency
      {noreply, State#state{tuples = Tuples ++ [Tuple]}}
  end;

%% @doc DEBUG function to release blocked processes
%% reply to the first blocked process, and then remove
%% the client from the state
handle_cast(release, State = #state{in_requests = []}) ->
  {noreply, State};
handle_cast(release, State = #state{in_requests = [Head | Tail]}) ->
  gen_server:reply(Head#in_request.client, debug_reply),
  {noreply, State#state{in_requests = Tail}}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle_info(_Info, _State) -> ok.