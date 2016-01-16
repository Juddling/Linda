-module(tuple_space).
-export([out/1, in/1, dump/0, release/0,
  start/0, stop/0]).
%% gen_server exports
-export([init/1, code_change/3, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).
-behavior(gen_server).

-record(state, {tuples, clients}).

dump() ->
  gen_server:call({global, ?MODULE}, dump).

release() ->
  gen_server:cast({global, ?MODULE}, release).

out(Tuple) ->
  gen_server:cast({global, ?MODULE}, {out, Tuple}).
%%  tuple_space(Bag ++ [Tuple]).

in(Template) ->
  gen_server:call({global, ?MODULE}, {in, Template}, infinity).
%%  % result needs to be sent to the kernel
%%  Result = fetch_tuple(Bag, Template),
%%  io:format('tuple returned was ~w with an in from template: ~w ~n', [Result, Template]),
%%
%%  % if we return a tuple, remove it from the bag, destructive in
%%  if
%%    Result == false ->
%%      tuple_space(Bag),
%%    true ->
%%      tuple_space(lists:delete(Result, Bag)).


%% gen_server functions

start() ->
  %% this will call init()
  %% returns: {ok, <process_id>}
  %% If ServerName={local,Name} the gen_server is registered locally as Name
  gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:stop({global, ?MODULE}).

init([]) ->
  {ok, #state{tuples = [], clients = []}}.

%% handle synchronous requests with handle_call()
handle_call(dump, _From, State=#state{tuples = Tuples}) ->
%%  io:format("dump request received from '~p' with current state '~p'~n",[From, State]),
  {reply, Tuples, State};
handle_call({in, Template}, From, State=#state{clients = Clients}) ->
  {noreply, State#state{clients = Clients ++ [From]}};
handle_call(Message, From, State) ->
  io:format("Generic call handler: '~p' from '~p' with current state '~p'~n",[Message, From, State]),
  {reply, ok, State}.

%% handle asynchronous requests with handle_cast
handle_cast({out, Tuple}, State=#state{tuples = Tuples}) ->
  io:format("tuple: '~p' has been outed~n",[Tuple]),
  %% maybe shouldn't append to the end of the list for efficiency
  {noreply, State#state{tuples = Tuples ++ [Tuple]}};

%% DEBUG function to release blocked processes
%% reply to the first blocked process, and then remove
%% the client from the state
handle_cast(release, State=#state{clients = []}) ->
  {noreply, State};

handle_cast(release, State=#state{clients = Clients}) ->
  gen_server:reply(hd(Clients), debug_reply),
  {noreply, State#state{clients = tl(Clients)}}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle_info(_Info, _State) -> ok.