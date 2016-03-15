-module(tuple_space).
-export([out/2, in/2, rd/2, dump/1, release/1, size/1,
  start/1, stop/1]).
% gen_server exports
-export([init/1, code_change/3, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).
-behavior(gen_server).

-record(state, {tuples, in_requests}).
-record(in_request, {client, template, destructive = true}).

server_identifier(Name) ->
  {global, Name}.

dump(Name) ->
  gen_server:call(server_identifier(Name), dump).

release(Name) ->
  gen_server:cast(server_identifier(Name), release).

size(Name) ->
  gen_server:call(server_identifier(Name), size).

%% @doc add a tuple to the space
out(Name, Tuple) when is_tuple(Tuple) ->
  gen_server:cast(server_identifier(Name), {out, Tuple}).

in(Name, Template) when is_tuple(Template) ->
  gen_server:call(server_identifier(Name), {read, Template, true}, infinity).

rd(Name, Template) when is_tuple(Template) ->
  gen_server:call(server_identifier(Name), {read, Template, false}, infinity).

% gen_server functions
start(Name) ->
  % this will call init()
  % returns: {ok, <process_id>}
  % server's name is registered globally
  gen_server:start_link(server_identifier(Name), ?MODULE, [], []).

stop(Name) ->
  gen_server:stop(server_identifier(Name)).

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

%% inform blocked clients when there is a deadlock
reply_deadlock([InReq | Tail]) ->
  gen_server:reply(InReq#in_request.client, deadlock),
  reply_deadlock(Tail);
reply_deadlock([]) ->
  ok.

%% @doc takes the server state containing all blocked in requests and returns
%% just the Pids of the blocked clients
blocked_clients(InRequests) ->
  lists:map(
    fun(Req) ->
      % client is of form {pid(),Tag}
      {Pid, _} = Req#in_request.client,
      Pid
    end, InRequests).

handle_call(blocked_clients, _From, State = #state{in_requests = InReqs}) ->
%%  io:format("blocked clients: ~p~n", [blocked_clients(InReqs)]),
  {reply, blocked_clients(InReqs), State};

handle_call(deadlock, _From, State) ->
  % inform all clients there is a deadlock
  reply_deadlock(State#state.in_requests),

  % all clients cleared, tuples remain in the space, TS lives on
  {reply, deadlock, State#state{in_requests = []}};
handle_call(size, _From, State = #state{tuples = Tuples}) ->
  {reply, length(Tuples), State};
%% @doc DEBUG function to show current stored tuples
handle_call(dump, _From, State = #state{tuples = Tuples}) ->
  {reply, Tuples, State};
handle_call({read, Template, IsDestructive}, From, State = #state{in_requests = InReqs, tuples = Tuples}) ->
  Result = template:fetch_tuple(Tuples, Template),
  if
    Result == false ->
      io:format("in request from ~p~n", [From]),

      % if a matching tuple isn't found, store this in request
      InRequest = #in_request{client = From, template = Template, destructive = IsDestructive},
      {noreply, State#state{in_requests = [InRequest|InReqs]}};
    true ->
      case IsDestructive of
        true ->
          % remove the tuple from state if it is destructive read
          {reply, Result, State#state{tuples = lists:delete(Result, Tuples)}};
        false ->
          {reply, Result, State}
      end
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
                    true -> Tuples;
                    _ -> [Tuple|Tuples]
                  end,

      {noreply, State#state{in_requests = lists:delete(InRequest, InRequests), tuples = NewTuples}};
    {nomatch} ->
      {noreply, State#state{tuples = [Tuple|Tuples]}}
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