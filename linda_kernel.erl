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
  case is_deadlock(Name, self()) of
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
  {Pid, Ref} = spawn_monitor(Fun),
  add_ref(Ref),
  add_process_to_tuple_space(Pid, TupleSpaceName),
  Pid.

%%spawn(Module, Function, List) ->
%%  %% store pid for any handle in the list
%%  {Pid, Ref} = spawn_monitor(Module, Function, List),
%%  add_ref(Ref),
%%  Pid.

add_ref(Ref) ->
  gen_server:cast({global, ?MODULE}, {add_ref, Ref}).

add_process_to_tuple_space(Pid, TupleSpace) ->
  gen_server:cast({global, ?MODULE}, {add_process, Pid, TupleSpace}).

process_down(State = #state{refs = Refs}, Ref, Pid) ->
  io:format("Process ~p went down", [Pid]),
  {noreply, State#state{refs = gb_sets:delete(Ref, Refs)}}.

%% gen_server functions
init(_Args) ->
  {ok, #state{tuple_spaces = dict:new(), refs = gb_sets:empty()}}.

handle_call({create, Name, From}, _From, State = #state{tuple_spaces = TupleSpaces}) ->
  {ok, Pid} = tuple_space:start(Name),
  io:format("create call received from : ~p", [From]),
  {reply, Pid, State#state{tuple_spaces = dict:append(Name, From, TupleSpaces)}};

handle_call(get_state, _From, State) ->
  {reply, State, State};

handle_call(set_state, _From, _OldState) ->


handle_call(_, _From, State) ->
  io:format("unsupported synchronous request~n"),
  {reply, unsupported, State}.

%% erlang monitors return a reference, we store this reference
%% in the linda kernel's state
handle_cast({add_ref, Ref}, State = #state{refs = Refs}) ->
  {noreply, State#state{refs = gb_sets:add(Ref, Refs)}};

handle_cast({add_process, Pid, TS}, State = #state{tuple_spaces = TupleSpaces}) ->
  {noreply, State#state{tuple_spaces = dict:append(TS, Pid, TupleSpaces)}};

handle_cast(_, State) ->
  io:format("unsupported asynchronous request~n"),
  {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% DOWN message received when monitored process terminates
handle_info({'DOWN', Ref, process, Pid, _}, State = #state{refs = Refs}) ->
  % check we're monitoring this process
  case gb_sets:is_element(Ref, Refs) of
    true ->
      process_down(State, Ref, Pid);
    false ->
      {noreply, State}
  end;

handle_info(_Info, _State) -> ok.

%% deadlock detection
is_deadlock(TupleSpaceName, From) ->
  State = get_kernel_state(),

  TupleSpaceDict = State#state.tuple_spaces,

  % dict:fetch assumes key is present in the dictionary
  Processes = dict:fetch(TupleSpaceName, TupleSpaceDict),

  NewTSDict = dict:store(TupleSpaceName, lists:delete(From, Processes), TupleSpaceDict),

  case length(Processes) of
    % two processes, one has been removed
    2 ->
      true;
    _ ->
      false
  end.

get_kernel_state() ->
  gen_server:call({global, ?MODULE}, get_state).

set_kernel_state(State) ->
  gen_server:call({global, ?MODULE}, {set_state, State}).