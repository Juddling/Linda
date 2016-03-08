-module(linda_kernel).

-export([start/0, stop/0,
  create_ts/2, remove_ts/1,
  in/2, out/2, rd/2,
  size/1, dump/1,
  spawn/2, deadlock/1]).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3, handle_info/2]).

-behavior(gen_server).

-record(state, {tuple_spaces, refs}).
-record(tuple_space, {clients, ts_referrers}).

%% consider using gb_sets as the data structure, or orddict?

server_identifier() -> {global, ?MODULE}.

start() ->
  gen_server:start_link(server_identifier(), ?MODULE, [], []).

stop() ->
  % is this enough?, should we go through and kill all tuple spaces that
  % we are aware of
  gen_server:stop(server_identifier()).

create_ts(Name, From) ->
  Pid = gen_server:call(server_identifier(), {create, Name, From}),
  Pid.

remove_ts(Name) ->
  %% do we allow TS to be stopped if other clients know about it?
  tuple_space:stop(Name).

in(Name, Template) when is_tuple(Template) ->
  io:format("in request received~n"),

%%  case is_deadlock(Name) of
%%    true ->
%%      erlang:error(deadlock);
%%    false ->
%%      tuple_space:in(Name, Template)
%%  end.

  tuple_space:in(Name, Template).

%% special case for if a tuple space handle is outed
%%out(Name, TSHandle = {ts_handle, TSName}) ->
%%  tuple_space:out(Name, TSHandle);

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

deadlock(InitialTS) ->
  gen_server:call(server_identifier(), {deadlock_detection, InitialTS}).

%%spawn(Module, Function, List) ->
%%  %% store pid for any handle in the list
%%  {Pid, Ref} = spawn_monitor(Module, Function, List),
%%  add_ref(Ref),
%%  Pid.

%% adds all referred TSs to the queue, unless they're already considered
%% potential deadlocks (this eliminates cycles)
queue_add(Queue, [], _) ->
  Queue;
queue_add(Queue, [Head | Tail], DeadlockedTSs) ->
  case lists:member(Head, DeadlockedTSs) of
    true ->
      queue_add(Queue, Tail, DeadlockedTSs);
    false ->
      queue_add(lists:append(Head, Queue), Tail, DeadlockedTSs)
  end.

%% asks a tuple space to check whether it is in deadlock / garbage
deadlock_detection([], TSDeadlocked, _State) ->
  % queue of tuple spaces to check is empty
  % given that this function only recurses if there is potential deadlock,
  % that means all tuple spaces in TSDeadlocked are in deadlock
  _ = lists:map(fun(TSName) -> gen_server:call({global, TSName}, deadlock) end, TSDeadlocked),
  deadlock;
deadlock_detection([TSName | TSQueueTail], TSDeadlocked, State) ->
  % get the kernels state for this TS
  TSState = state_get_tuple_space(State, TSName),
  % all processes which are aware of this tuple space
  ClientsAware = TSState#tuple_space.clients,

  DetectionState = gen_server:call({global, TSName}, {detect, ClientsAware}),

  case DetectionState of
    all_clients_blocked ->
      % add tuple spaces that refer to this one to the queue of tuple spaces to check
      NewQueue = queue_add(TSQueueTail, TSState#tuple_space.ts_referrers, TSDeadlocked),

      deadlock_detection(NewQueue, [TSName | TSDeadlocked], State);
    none ->
      % in this case there is definitely no deadlock as there in an unblocked
      % process that can write to this TS, and this TS is referred to by the previous
      % TSs
      no_deadlock
  end.


process_down(State = #state{refs = Refs}, Ref, Pid) ->
  StateClientRemoved = state_remove_client(State, Pid),
  NewState = StateClientRemoved#state{refs = gb_sets:delete(Ref, Refs)},
  {noreply, NewState}.

%% HELPER FUNCTIONS TO MANAGE KERNEL STATE

state_add_tuple_space(State = #state{tuple_spaces = TSs}, TSName, Client) ->
  TSState = #tuple_space{clients = [Client], ts_referrers = []},
  State#state{tuple_spaces = dict:store(TSName, TSState, TSs)}.

%% add a new client
state_add_client(State = #state{tuple_spaces = TSs}, TSName, Client) ->
  TSState = state_get_tuple_space(State, TSName),
  NewTSState = TSState#tuple_space{clients = lists:append([Client], TSState#tuple_space.clients)},
  io:format("adding client, current ts state~p~n", [NewTSState]),
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
  NewTSState = TSState#tuple_space{ts_referrers = lists:append(Referrer, TSState#tuple_space.ts_referrers)},
  State#state{tuple_spaces = dict:store(TSName, NewTSState, TSs)}.

state_get_tuple_space(State, TSName) ->
  dict:fetch(TSName, State#state.tuple_spaces).

%% gen_server functions
init(_Args) ->
  {ok, #state{tuple_spaces = dict:new(), refs = gb_sets:empty()}}.

handle_call({create, Name, From}, _From, State) ->
  {ok, Pid} = tuple_space:start(Name),
  io:format("create call received from : ~p, and handled by ~p ~n", [From, self()]),
  {reply, Pid, state_add_tuple_space(State, Name, From)};

%% erlang monitors return a reference, we store this reference
%% in the linda kernel's state
handle_call({spawn, Fun, TSName}, _From, State = #state{refs = Refs}) ->
  {Pid, Ref} = spawn_monitor(Fun),
  io:format("new process being made by process ~p~n", [self()]),
  io:format("new process ~p added to TS: ~p~n", [Pid, TSName]),
  % append to tuple space dict adds knowledge of which processes know about which TS
  NewState = state_add_client(State, TSName, Pid),
  {reply, Pid, NewState#state{refs = gb_sets:add(Ref, Refs)}};

handle_call({deadlock_detection, InitialTupleSpace}, _From, State) ->
  {reply, deadlock_detection([InitialTupleSpace], [], State), State};

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

handle_info(_Info, _State) -> ok.