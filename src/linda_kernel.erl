-module(linda_kernel).

-export([start/0, stop/0,
  create_ts/0, remove_ts/1,
  in/2, out/2, rd/2,
  size/1, dump/1,
  spawn/2, spawn/4, deadlock/1, deadlock/0]).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3, handle_info/2]).

-behavior(gen_server).

-record(state, {tuple_spaces, refs}).
-record(tuple_space, {clients, ts_referrers}).

% how often in milliseconds the kernel checks for deadlocks
-define(DEADLOCK_INTERVAL, 1000).

%% consider using gb_sets as the data structure, or orddict?

server_identifier() -> {global, ?MODULE}.

start() ->
  gen_server:start_link(server_identifier(), ?MODULE, [], []).

stop() ->
  % is this enough?, should we go through and kill all tuple spaces that
  % we are aware of
  gen_server:stop(server_identifier()).

%% creates a new tuple space with a unique name, returns the new generated name
create_ts() ->
  Name = make_ref(),
  _ = gen_server:call(server_identifier(), {create, Name, self()}),
  Name.

remove_ts(Name) ->
  %% do we allow TS to be stopped if other clients know about it?
  tuple_space:stop(Name).

in(Name, Template) when is_tuple(Template) ->
%%  case is_deadlock(Name) of
%%    true ->
%%      erlang:error(deadlock);
%%    false ->
%%      tuple_space:in(Name, Template)
%%  end.

  tuple_space:in(Name, Template).

%% special case for if a tuple space handle is outed
out(Name, TSHandle = {ts_handle, TSName}) ->
  tuple_space:out(Name, TSHandle),
  % make sure the kernel knows Name refers to TSName
  gen_server:call(server_identifier(), {add_referrer, TSName, Name});

out(Name, Tuple) when is_tuple(Tuple) ->
  tuple_space:out(Name, Tuple).

rd(Name, Tuple) when is_tuple(Tuple) ->
  tuple_space:rd(Name, Tuple).

size(Name) ->
  tuple_space:size(Name).

dump(Name) ->
  tuple_space:dump(Name).

%% process creation
spawn(TupleSpaceName, Fun) ->
  gen_server:call(server_identifier(), {spawn, Fun, TupleSpaceName}).

spawn(TupleSpaceName, Module, Function, Args) ->
  gen_server:call(server_identifier(), {spawn, {Module, Function, Args}, TupleSpaceName}).

deadlock(InitialTS) ->
  gen_server:call(server_identifier(), {deadlock_detection, InitialTS}).

deadlock() ->
  gen_server:call(server_identifier(), {deadlock_detection}).

%% adds all referred TSs to the queue, unless they're already considered
%% potential deadlocks (this eliminates cycles)
queue_add(Queue, [], _) ->
  Queue;
queue_add(Queue, [Head | Tail], DeadlockedTSs) ->
  case lists:member(Head, DeadlockedTSs) of
    true ->
      queue_add(Queue, Tail, DeadlockedTSs);
    false ->
      queue_add([Head | Queue], Tail, DeadlockedTSs)
  end.

sets_equal(SetA, SetB) ->
  sets:is_subset(SetA, SetB) andalso sets:is_subset(SetB, SetA).

%% asks a tuple space to check whether it is in deadlock / garbage
deadlock_detection([], TSDeadlocked, ClientAware, ClientsBlocked, _State) ->
  IsDeadlock = sets_equal(ClientAware, ClientsBlocked),

%%  io:format("is deadlock?~p~n", [IsDeadlock]),
%%  io:format("Aware: ~p, Blocked: ~p~n", [sets:to_list(ClientAware), sets:to_list(ClientsBlocked)]),

  case IsDeadlock of
    true ->
%%      io:format("deadlock found for the following tuple spaces: ~p~n", [TSDeadlocked]),
      _ = lists:map(fun(TSName) -> gen_server:call({global, TSName}, deadlock) end, TSDeadlocked),
      deadlock;
    false ->
      no_deadlock
  end;
deadlock_detection([TSName | TSQueueTail], TSDeadlocked, ClientAware, ClientsBlocked, State) ->
  % get the kernels state for this TS
  TSState = state_get_tuple_space(State, TSName),

%%  io:format("adding clients from ~p~n", [TSName]),

  % all processes which are aware of this tuple space
  TSClients = sets:from_list(TSState#tuple_space.clients),
  BlockedClientsList = gen_server:call({global, TSName}, blocked_clients),

%%  io:format("blocked clients ts name: ~p, clients: ~p~n", [TSName, BlockedClientsList]),

  TSClientsBlocked = sets:from_list(BlockedClientsList),

  NewQueue = queue_add(TSQueueTail, TSState#tuple_space.ts_referrers, TSDeadlocked),

  deadlock_detection(NewQueue, [TSName|TSDeadlocked], sets:union(ClientAware,TSClients),
    sets:union(ClientsBlocked,TSClientsBlocked), State).

process_down(State = #state{refs = Refs}, Ref, Pid) ->
  StateClientRemoved = state_remove_client(State, Pid),
  NewState = StateClientRemoved#state{refs = gb_sets:delete(Ref, Refs)},
  {noreply, NewState}.


schedule_deadlock_detection() ->
  erlang:send_after(?DEADLOCK_INTERVAL, self(), {deadlock_detection}).

%% --------------------
%% Helper functions to manage the state of the kernel
%% --------------------
state_add_tuple_space(State = #state{tuple_spaces = TSs}, TSName, Client) ->
  TSState = #tuple_space{clients = [Client], ts_referrers = []},
  State#state{tuple_spaces = dict:store(TSName, TSState, TSs)}.

%% add a new client
state_add_client(State = #state{tuple_spaces = TSs}, TSName, Client) ->
  TSState = state_get_tuple_space(State, TSName),
  NewTSState = TSState#tuple_space{clients = [Client | TSState#tuple_space.clients]},
%%  io:format("adding client, current ts state~p~n", [NewTSState]),
  State#state{tuple_spaces = dict:store(TSName, NewTSState, TSs)}.

%% removes client from tuple space knowledge
%% this function is called when the process/client dies
state_remove_client(State = #state{tuple_spaces = TSs}, Client) ->
  dict = element(1, TSs),
  NewTSState = dict:map(
    fun(_TSName, TSState) ->
      TSState#tuple_space{clients = lists:delete(Client, TSState#tuple_space.clients)}
    end, TSs),
  dict = element(1, NewTSState),
  State#state{tuple_spaces = NewTSState}.

state_add_referrer(State = #state{tuple_spaces = TSs}, TSName, Referrer) ->
  TSState = state_get_tuple_space(State, TSName),
  NewTSState = TSState#tuple_space{ts_referrers = [Referrer | TSState#tuple_space.ts_referrers]},
  State#state{tuple_spaces = dict:store(TSName, NewTSState, TSs)}.

state_get_tuple_space(State, TSName) ->
  % io:format("tuple space state requested, server state is : ~p~n", [State]),
  dict:fetch(TSName, State#state.tuple_spaces).

%% --------------------
%% gen_server functions
%% --------------------
init(_Args) ->
  schedule_deadlock_detection(),
  {ok, #state{tuple_spaces = dict:new(), refs = gb_sets:empty()}}.

handle_call({create, Name, From}, _From, State) ->
  {ok, Pid} = tuple_space:start(Name),
  io:format("create call received from : ~p, and handled by ~p ~n", [From, self()]),
  _ = erlang:monitor(process, From),
  {reply, Pid, state_add_tuple_space(State, Name, From)};

%% erlang monitors return a reference, we store this reference
%% in the linda kernel's state
handle_call({spawn, Fun, TSName}, _From, State = #state{refs = Refs}) ->
  {Pid, Ref} =
    case Fun of
      {Module, Function, Args} ->
        spawn_monitor(Module, Function, Args);
      Fun ->
        spawn_monitor(Fun)
    end,
%%  io:format("new process being made by process ~p~n", [self()]),
%%  io:format("new process ~p added to TS: ~p~n", [Pid, TSName]),
  % append to tuple space dict adds knowledge of which processes know about which TS
  NewState = state_add_client(State, TSName, Pid),
  {reply, Pid, NewState#state{refs = gb_sets:add(Ref, Refs)}};



%%handle_call({deadlock_detection, InitialTupleSpace}, _From, State) ->
%%  {reply, deadlock_detection([InitialTupleSpace], [], State), State};

%% update the kernel's knowledge, Referrer has a handle which points to TSName
handle_call({add_referrer, TSName, Referrer}, _From, State) ->
  {reply, ok, state_add_referrer(State, TSName, Referrer)};

handle_call(_, _From, State) ->
  io:format("unsupported synchronous request~n"),
  {reply, unsupported, State}.

handle_cast(_, State) ->
  io:format("unsupported asynchronous request~n"),
  {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% DOWN message received when monitored process terminates
handle_info({'DOWN', Ref, process, Pid, _}, State = #state{refs = Refs}) ->
  io:format("process ~p has terminated~n", [Pid]),
  % check we're monitoring this process
  case gb_sets:is_element(Ref, Refs) of
    true ->
      process_down(State, Ref, Pid);
    false ->
      {noreply, State}
  end;

handle_info({deadlock_detection}, State) ->
%%  io:format("checking for deadlock~n"),
  schedule_deadlock_detection(),

  TSNames = dict:fetch_keys(State#state.tuple_spaces),
  _ = lists:map(
    fun(TSName) ->
      deadlock_detection([TSName], [], sets:new(), sets:new(), State)
    end, TSNames),

  {noreply, State};

handle_info(_Info, _State) -> ok.