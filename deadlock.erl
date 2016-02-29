%%%-------------------------------------------------------------------
%%% @author Juddling
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%% Simplest example of deadlock
%%% @end
%%% Created : 15. Feb 2016 9:34 PM
%%%-------------------------------------------------------------------
-module(deadlock).
-author("Juddling").

%% API
-export([main/0, start/0]).

start() ->
  spawn(?MODULE, main, []).

main() ->
  % start the linda kernel
  linda_kernel:start(),

  % create a tuple space
  linda_kernel:create_ts(deadlock_ts, self()),

  % spawn a process which is also aware of this tuple space
  linda_kernel:spawn(deadlock_ts, fun() -> io:format("running new process...~n"), timer:sleep(1000), io:format("anon process done...~n") end),

  % request an integer, this is a deadlock as soon as the other process dies
  spawn(fun() -> linda_kernel:in(deadlock_ts, {integer}) end).