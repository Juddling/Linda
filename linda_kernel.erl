-module(linda_kernel).

-export([start/0,
  create_ts/2, remove_ts/1,
  in/2, out/2, rd/2,
  size/1, dump/1,
  spawn/2]).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3, handle_info/2]).

-behavior(gen_server).

-record(state, {tuple_spaces, refs}).

%% consider using gb_sets as the data structure, or orddict?

start() ->
  gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

create_ts(Name, From) ->
  Pid = gen_server:call({global, ?MODULE}, {create, Name, From}),
  Pid.

remove_ts(Name) ->
  %% do we allow TS to be stopped if other clients know about it?
  tuple_space:stop(Name).

in(Name, Template) when is_tuple(Template) ->
  io:format("in request received~n"),
  case is_deadlock(Name) of
    true ->
      erlang:error(deadlock);
    false ->
      tuple_space:in(Name, Template)
  end.

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

%%spawn(Module, Function, List) ->
%%  %% store pid for any handle in the list
%%  {Pid, Ref} = spawn_monitor(Module, Function, List),
%%  add_ref(Ref),
%%  Pid.

process_down(State = #state{refs = Refs}, Ref, Pid) ->
  io:format("reference removed to ~p~n", [Pid]),
  {noreply, State#state{refs = gb_sets:delete(Ref, Refs)}}.

%% deadlock detection
is_deadlock(TupleSpaceName) ->
  gen_server:call(server_identifier(), {is_deadlock, TupleSpaceName}).

server_identifier() -> {global, ?MODULE}.

%% gen_server functions
init(_Args) ->
  {ok, #state{tuple_spaces = dict:new(), refs = gb_sets:empty()}}.

handle_call({create, Name, From}, _From, State = #state{tuple_spaces = TupleSpaces}) ->
  {ok, Pid} = tuple_space:start(Name),
  io:format("create call received from : ~p, and handled by ~p ~n", [From, self()]),
  {reply, Pid, State#state{tuple_spaces = dict:append(Name, From, TupleSpaces)}};

%% deadlock detection
handle_call({is_deadlock, TupleSpaceName}, From, State = #state{tuple_spaces = TupleSpaceDict}) ->
  % dict:fetch assumes key is present in the dictionary
  Processes = dict:fetch(TupleSpaceName, TupleSpaceDict),

  NewTSDict = dict:store(TupleSpaceName, lists:delete(From, Processes), TupleSpaceDict),

  ProcessCount = length(Processes),

  io:format("checking for deadlock, process count: ~p~n", [ProcessCount]),

  IsDeadlock = case ProcessCount of
    % two processes, one has been removed
    1 ->
      true;
    _ ->
      false
  end,

  {reply, IsDeadlock, State#state{tuple_spaces = NewTSDict}};

%% erlang monitors return a reference, we store this reference
%% in the linda kernel's state
handle_call({spawn, Fun, TSName}, _From, State = #state{refs = Refs, tuple_spaces = TupleSpaces}) ->
  {Pid, Ref} = spawn_monitor(Fun),
  io:format("new process being made by process ~p~n", [self()]),
  io:format("new process ~p added to TS: ~p~n", [Pid, TSName]),
  {reply, Pid, State#state{tuple_spaces = dict:append(TSName, Pid, TupleSpaces), refs = gb_sets:add(Ref, Refs)}};

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