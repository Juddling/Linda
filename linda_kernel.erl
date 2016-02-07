-module(linda_kernel).

-export([create_ts/1, remove_ts/1,
  in/2, out/2, rd/2, size/1, dump/1]).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3, handle_info/2]).

-behavior(gen_server).

-record(state, {tuple_spaces}).

start() ->
  % create global tuple space
  ok.

create_ts(Name) ->
  %% TODO: store who created this TS, they know about it
  %% TODO: handle when ts already exists
  {_, Pid} = tuple_space:start(Name),
  Pid.

remove_ts(Name) ->
  %% do we allow TS to be stopped if other clients know about it?
  tuple_space:stop(Name).

in(Name, Template) when is_tuple(Template) ->
  tuple_space:in(Name, Template).

out(Name, Tuple) when is_tuple(Tuple) ->
  tuple_space:out(Name, Tuple).

rd(Name, Tuple) when is_tuple(Tuple) ->
  tuple_space:rd(Name, Tuple).

size(Name) ->
  tuple_space:size(Name).

dump(Name) ->
  tuple_space:dump(Name).

%% gen_server functions
init(_Args) ->
  {ok, #state{tuple_spaces = []}}.

handle_call({create, Name}, _From, State = #state{tuple_spaces = TupleSpaces}) ->
  tuple_space:start(Name),
  {reply, ok, State#state{tuple_spaces = TupleSpaces ++ [Name]}}.

handle_cast(_, State) ->
  {not_implemented, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle_info(_Info, _State) -> ok.