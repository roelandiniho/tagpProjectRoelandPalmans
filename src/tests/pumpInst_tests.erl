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
%%  unregister(pumpInst1),
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
  ?assert(erlang:is_process_alive(whereis(pumpInst1))).

test_switchOff() ->
  SwitchOff = pumpInst:switch_off(pumpInst1),

  ?assertEqual(switchOff, SwitchOff).

test_switchOn() ->
  SwitchOn = pumpInst:switch_on(pumpInst1),

  ?assertEqual(switchOn, SwitchOn).

test_switchIsOn() ->
  pumpInst:switch_off(pumpInst1),
  {ok,IsOff} = msg:get(pumpInst1, isOn),

  ?assertEqual(off,IsOff),
  pumpInst:switch_on(pumpInst1),
  {ok,IsOn} = msg:get(pumpInst1, isOn),

  ?assertEqual(on, IsOn).

test_flow_influence() ->
  SwitchOn = pumpInst:switch_on(pumpInst1),
  {ok,FunctionOn} = pumpInst:get_flow_influence(pumpInst1),
  ?assertNotEqual([],FunctionOn),

  pumpInst:switch_off(whereis(pumpInst1)),
  {ok,FunctionOff} = pumpInst:get_flow_influence(pumpInst1),
  ?assertNotEqual([],FunctionOff),
  ?assertNotEqual(FunctionOff, FunctionOn).