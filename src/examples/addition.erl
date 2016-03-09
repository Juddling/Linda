-module(addition).
-export([init/0, start/0, add/1]).

start() ->
  spawn(?MODULE, init, []).

add(TupleSpaceName) ->
  {One} = tuple_space:in(TupleSpaceName, {integer}),

  io:format("~p received a one of: ~p~n", [self(), One]),

  {Two} = tuple_space:in(TupleSpaceName, {integer}),

  io:format("~p received a two of: ~p~n", [self(), Two]),

  tuple_space:out(TupleSpaceName, {One + Two}),

  add(TupleSpaceName).

init() ->
  N = 50,
  Workers = 2,

  linda_kernel:start(),

  % create a tuple space
  TSName = linda_kernel:create_ts(self()),

  % add numbers to TS
  lists:map(fun(X) -> linda_kernel:out(TSName, {X}) end, lists:seq(1, N)),

  % wait for the numbers to be added
  timer:sleep(100),

  linda_kernel:spawn(TSName, ?MODULE, add, [TSName]),

  add(TSName).

  % lists:map(fun(_X) -> linda_kernel:spawn(TSName, ?MODULE, add, [TSName]) end, lists:seq(1, Workers)).