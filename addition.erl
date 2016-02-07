-module(addition).
-export([init/0, execute/2, supervisor/2, add/1]).

-define(ADDITION_TS, addition_ts).

add(Pid) when is_pid(Pid) ->
  {One} = tuple_space:in(?ADDITION_TS, {integer}),

  io:format("~p received a one of: ~p~n", [self(), One]),

  {Two} = tuple_space:in(?ADDITION_TS, {integer}),

  io:format("~p received a two of: ~p~n", [self(), Two]),

  tuple_space:out(?ADDITION_TS, {One + Two}),

  %% inform the supervisor process it is complete
  Pid ! done.

execute(Workers, Pid) when is_pid(Pid) ->
  lists:map(fun(_X) -> spawn(?MODULE, add, [Pid]) end, lists:seq(1, Workers)).

init() ->
  N = 10,
  _ = linda_kernel:create_ts(?ADDITION_TS),
  self() ! "init",
  lists:map(fun(X) -> tuple_space:out(?ADDITION_TS, {X}) end, lists:seq(1, N)),
  spawn(?MODULE, supervisor, [N, self()]).

supervisor(TupleCount, ReplyPid) ->
  receive
    done ->
      %% this means a worker has just finished
      NewTupleCount = TupleCount - 1,
      ReplyPid ! NewTupleCount,

      case NewTupleCount of
        0 ->
          %% last tuple in the ts, we can return it
          ReplyPid ! lollolololol,
          ReplyPid ! tuple_space:in(?ADDITION_TS, {integer});
        _ ->
          % TODO: don't just call the function here, spawn off a new set of workers
          % or reply to the works to let them know they can recurse
          add(self()),
          supervisor(NewTupleCount, ReplyPid)
      end
  end.