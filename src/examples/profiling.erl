-module(profiling).
-author("Juddling").

%% API
-export([main/0, start/0]).

start() ->
  spawn(?MODULE, main, []).

out_multiple(TS, 0) ->
  done;
out_multiple(TS, N) ->
  linda_kernel:out(TS, {N}),
%%  io:format("Adding tuple ~p~n", [N]),
  out_multiple(TS, N - 1).

main() ->
  eprof:start(),
  eprof:start_profiling([self()]),

  linda_kernel:start(),

  TS = linda_kernel:create_ts(),
  N = 5000,

  out_multiple(TS, N),
  _ = linda_kernel:in(TS, {1}),

  eprof:stop_profiling(),
  eprof:log('results2.txt'),
  eprof:analyze(total).