-module(stable_marriage).
-author("Juddling").

%% API
-export([man/2, woman/2, main/0]).

-record(man_state, {name, current_woman = none, preferences}).
-record(woman_state, {name, current_man = none, preferences}).

man(TS, ManState = #man_state{current_woman = CurrentWoman, preferences = Preferences, name = ManName}) ->
  case CurrentWoman of
    none ->
      % if we have no woman, propose to the first
      FirstWoman = lists:nth(1, Preferences),
      io:format("sending out initial proposal to ~p~n", [FirstWoman]),
      linda_kernel:out(TS, {proposal, FirstWoman, ManName}),

      man(TS, ManState#man_state{current_woman = FirstWoman});
    _ ->
      % if we do have a woman, wait for a rejection
      case linda_kernel:in(TS, {rejection, ManName}) of
        {rejection, _} ->
          io:format("~p has been rejected~n", [ManName]),

          % now propose to the next best
          NextBest = next_best_woman(CurrentWoman, Preferences),
          linda_kernel:out(TS, {proposal, NextBest, ManName}),

          man(TS, ManState#man_state{current_woman = NextBest});
        deadlock ->
          io:format("~p got married to ~p~n", [ManName, CurrentWoman])
      end
  end.

woman(TS, WomanState = #woman_state{current_man = CurrentMan, preferences = Preferences, name = WomanName}) ->
  % wait for a proposal
  case linda_kernel:in(TS, {proposal, WomanName, string}) of
    {proposal, _, ProposingMan} ->
      io:format("~p received a proposal from ~p~n", [WomanName, ProposingMan]),
      case CurrentMan of
        none ->
          % if we don't have a man, then accept
          woman(TS, WomanState#woman_state{current_man = ProposingMan});
        _ ->
          % is the man who proposed better than our current man?
          case better_match(CurrentMan, ProposingMan, Preferences) of
            true ->
              % reject our current man
              linda_kernel:out(TS, {rejection, CurrentMan}),
              % change our current man to the new guy
              woman(TS, WomanState#woman_state{current_man = ProposingMan});
            _ ->
              % reject the proposing man
              linda_kernel:out(TS, {rejection, ProposingMan}),
              % staying with the same man, so state doesn't change
              woman(TS, WomanState)
          end
      end;
    deadlock ->
      io:format("~p got married to ~p~n", [WomanName, CurrentMan])
  end.

%% search through the man's list of preferences to find the woman after CurrentWoman
next_best_woman(CurrentWoman, [H|Tail]) ->
  case H == CurrentWoman of
    true ->
      [NextWoman|_] = Tail,
      NextWoman;
    _ ->
      next_best_woman(CurrentWoman, Tail)
  end.

%% does the woman prefer ProposingMan to CurrentMan?
better_match(CurrentMan, ProposingMan, [H|Tail]) ->
  case H of
    ProposingMan ->
      % we found the new guy first, so he's a better match
      true;
    CurrentMan ->
      % our current man is higher in the preferences, so the new guy is not better
      false;
    _ ->
      better_match(CurrentMan, ProposingMan, Tail)
  end.

shuffle(ListOfNames) ->
  [X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- ListOfNames])].

main() ->
  linda_kernel:start(),

  TS = linda_kernel:create_ts(),

  Men = ["AIDAN", "JADEN", "CADEN", "ETHAN", "CALEB", "DYLAN", "JACOB", "JORDAN", "LOGAN", "HAYDEN", "CONNOR", "RYAN",
    "MORGAN", "CAMERON", "ANDREW", "JOSHUA", "NOAH", "MATTHEW", "ADDISON", "ASHTON"],

  Women = ["MADISON", "EMMA", "ABIGAIL", "RILEY", "CHLOE", "HANNAH", "ALEXIS", "ISABELLA", "MACKENZIE", "TAYLOR",
    "OLIVIA", "HAILEY", "PAIGE", "EMILY", "GRACE", "AVA", "AALIYAH", "ALYSSA", "FAITH", "BRIANNA"],

  % spawn a process for each man
  lists:map(
    fun (Man) ->
      ManState = #man_state{name = Man, preferences = shuffle(Women)},
      linda_kernel:spawn(TS, ?MODULE, man, [TS, ManState])
    end, Men),

  % spawn a process for each woman
  lists:map(
    fun (Woman) ->
      WomanState = #woman_state{name = Woman, preferences = shuffle(Men)},
      linda_kernel:spawn(TS, ?MODULE, woman, [TS, WomanState])
    end, Women),

  % block this process on purpose so that deadlock happens
  linda_kernel:in(TS, {done}).