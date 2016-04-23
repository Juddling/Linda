%%%-------------------------------------------------------------------
%%% @author Juddling
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%% Simplest example of deadlock
%%% @end
%%% Created : 15. Feb 2016 9:34 PM
%%%-------------------------------------------------------------------
-module(deadlock_example).
-author("Juddling").

%% API
-export([main/0, start/0]).

start() ->
  spawn(?MODULE, main, []).

main() ->
  % start the linda kernel
  linda_kernel:start(),

  % create a tuple space
  TS1 = linda_kernel:create_ts(),

  % spawn a process which is also aware of this tuple space
  linda_kernel:spawn(TS1, fun() -> io:format("running new process...~n"), timer:sleep(500), io:format("anon short process done...~n") end),

  % spawn a process which is also aware of this tuple space
  linda_kernel:spawn(TS1, fun() ->
    io:format("running new longer process...~n"),
    timer:sleep(5000),
    % linda_kernel:out(deadlock_ts, {3}),
    io:format("anon long process done...~n")
                                  end),

  % request an integer, this is a deadlock as soon as the other process dies
  Result = linda_kernel:in(TS1, {integer}),
  io:format("RESPONSE RECEIVED: ~p~n", [Result]).