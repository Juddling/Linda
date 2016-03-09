%%%-------------------------------------------------------------------
%%% @author Juddling
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Mar 2016 9:55 PM
%%%-------------------------------------------------------------------
-module(deadlock_cycle).
-author("Juddling").

-export([main/0, start/0, p1/0]).

start() ->
  spawn(?MODULE, main, []).

p1() ->
  io:format("p1 started").

main() ->
  linda_kernel:start(),

  TS1 = linda_kernel:create_ts(),
  TS2 = linda_kernel:create_ts(),

  % create a cycle TS2 has a tuple space handle for TS1 and vice versa
  linda_kernel:out(TS2, {ts_handle, TS1}),
  linda_kernel:out(TS1, {ts_handle, TS2}),

  % wait for the tuples to be added
  timer:sleep(10),

  _ = linda_kernel:in(TS1, {integer}),

  io:format("deadlock_cycle:main() finished~n").