-module(addition).
-export([init/0, start/0, add/1]).

start() ->
  spawn(?MODULE, init, []).

%% takes two numbers from a TS, adds them and puts it back,
%% this will cause deadlock either:
%% - when the TS is empty
%% - or multiple processes hold one number each
add(TupleSpaceName) ->
  {One} = tuple_space:in(TupleSpaceName, {integer}),

  io:format("~p received a one of: ~p~n", [self(), One]),

  {Two} = tuple_space:in(TupleSpaceName, {integer}),

  io:format("~p received a two of: ~p~n", [self(), Two]),

  tuple_space:out(TupleSpaceName, {One + Two}),

  add(TupleSpaceName).

init() ->
  N = 50,
%%  Workers = 2,

  linda_kernel:start(),

  % create a tuple space
  TSName = linda_kernel:create_ts(),

  % add numbers to TS
  lists:map(fun(X) -> linda_kernel:out(TSName, {X}) end, lists:seq(1, N)),

  % wait for the numbers to be added
  timer:sleep(100),

  % spawn a new process to run the add function
  linda_kernel:spawn(TSName, ?MODULE, add, [TSName]),
  % make the main process run add
  add(TSName).

%%  lists:map(fun(_X) -> linda_kernel:spawn(TSName, ?MODULE, add, [TSName]) end, lists:seq(1, Workers)).