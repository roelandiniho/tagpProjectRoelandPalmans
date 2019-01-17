%%%-------------------------------------------------------------------
%%% @author roeland
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Jan 2019 13:38
%%%-------------------------------------------------------------------
-module(circuit_tests).
-author("roeland").

-include("proper/proper.hrl").

%% API
-export([random_process/0, unregisterAll/0]).

prop_create_new_atom() ->
  ?FORALL({X,Y}, {atom(), atom()}, list_to_atom(lists:flatten(io_lib:format("~p~p", [X, Y]))) =:= circuit:create_atom(X,Y)).

prop_checkAvailableProcessName_NonRegistered() ->
  ?FORALL(X, atom(), circuit:create_atom(X,1) =:= circuit:checkAvailableProcessName(X)).

prop_checkAvailableProcessName_RandomAmountRegistered() ->
  ?FORALL(X, atom(),
    begin
      Number = random:uniform(10),
      create_random_processes(X, 1, Number),
      A = circuit:checkAvailableProcessName(X),
      B = circuit:create_atom(X,Number+1),
      unregister_processes(X, 1, Number),
      A=:=B
    end).

create_random_processes(X, Counter, Number) when Counter =< Number->
  Pid = spawn(?MODULE, random_process, []),
  register(circuit:create_atom(X, Counter), Pid),
  create_random_processes(X, Counter + 1, Number);
create_random_processes(_, _, _) -> ok.

random_process() ->
  receive
    {_} -> random_process()
  end.

unregister_processes(X, Counter, Number) when Counter =< Number->
  unregister(circuit:create_atom(X, Counter)),
  unregister_processes(X, Counter + 1, Number);
unregister_processes(_, _, _) -> ok.

prop_getResourceTypPid() ->
  ?FORALL(X, atom(),
    begin
        case random:uniform(2) of
          2 -> Y = {ok, X};
          1 -> Y = X
      end,
      X =:=circuit:getResourceTypPid(Y)
    end).


prop_create() ->
  ?FORALL(X, atom(),
    begin
      circuit:create(),
      timer:sleep(2000),
      unregisterAll(),
      X =:= X
    end).

unregisterAll() ->
  unregisterTypes([pipeTyp, pumpTyp, fluidumTyp, flowMeterTyp, heatExchangerTyp]),
  unregisterInstances([pipeInst, pumpInst, heatExchangerInst, flowMeterInst, fluidumInst]).

unregisterTypes([]) -> ok;
unregisterTypes([Type| Iter]) ->
  io:format("Type = ~p ~n", [Type]),
  case whereis(Type) of
    'undefined'-> ok;
    _ -> unregister(Type)
  end,
  unregisterTypes(Iter).

unregisterInstances([]) -> ok;
unregisterInstances([X| Iter]) ->
  unregisterProcess(X, 1),
  unregisterInstances(Iter).

unregisterProcess(Instance, Counter)->
  io:format("Instance = ~p~p ~n", [Instance, Counter]),
  case whereis(circuit:create_atom(Instance, Counter)) of
    'undefined'-> ok;
    _ -> unregister(circuit:create_atom(Instance, Counter)),
      unregisterProcess(Instance, Counter+ 1)
end.