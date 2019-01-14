%%%-------------------------------------------------------------------
%%% @author roeland
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Jan 2019 17:55
%%%-------------------------------------------------------------------
-module(pumpInst_tests).
-author("roeland").

-include_lib("eunit/include/eunit.hrl").

%% API
-export([]).

setup() ->
  meck:new(survivor2),
    meck:expect(survivor2, entry,
    fun(_) ->
      pumpTyp_created
    end),
  {_,PumpTyp} = pumpTyp:create(),
  register(pumpTyp, PumpTyp),
%%  meck:new(resource_type),
%%  meck:expect(resource_type, get_initial_state,
%%    fun(_,_,_) ->
%%      {ok, #{cList => [connectorIn,connectorOut],
%%        chambers => [chamber],
%%        resInst => pipeInst,typeOptions => []}}
%%    end),
  {ok,PumpInst} = pumpInst:create(host, PumpTyp, pipeInstPid, fun(W) -> W end),
  register(pumpInst, PumpInst).

cleanup(_) ->
  unregister(pumpTyp),
  unregister(pumpInst),
  meck:unload().

pumpInst_test_() ->
  {setup,
    fun setup/0,
    fun cleanup/1,
    [{inorder,
      [fun test_create/0,
        fun test_switchOff/0,
        fun test_switchOn/0,
        fun test_switchIsOn/0,
        fun test_flow_influence/0
      ]}]}.

test_create() ->
  ?assert(erlang:is_process_alive(whereis(pumpTyp))),
  ?assert(erlang:is_process_alive(whereis(pumpInst))).

test_switchOff() ->
  SwitchOff = pumpInst:switch_off(whereis(pumpInst)),

  ?assertEqual(switchOff, SwitchOff).

test_switchOn() ->
  SwitchOn = pumpInst:switch_on(whereis(pumpInst)),

  ?assertEqual(switchOn, SwitchOn).

test_switchIsOn() ->
  pumpInst:switch_off(whereis(pumpInst)),
  {ok,IsOff} = msg:get(whereis(pumpInst), isOn),

  ?assertEqual(off,IsOff),
  pumpInst:switch_on(whereis(pumpInst)),
  {ok,IsOn} = msg:get(whereis(pumpInst), isOn),

  ?assertEqual(on, IsOn).

test_flow_influence() ->
  pumpInst:switch_on(whereis(pumpInst)),
  {ok,FunctionOn} = pumpInst:flow_influence(whereis(pumpInst)),
  ?assertNotEqual([],FunctionOn),

  pumpInst:switch_off(whereis(pumpInst)),
  {ok,FunctionOff} = pumpInst:flow_influence(whereis(pumpInst)),
  ?assertNotEqual([],FunctionOff),
  ?assertNotEqual(FunctionOff, FunctionOn).