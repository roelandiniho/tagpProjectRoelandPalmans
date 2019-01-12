%%%-------------------------------------------------------------------
%%% @author roeland
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Jan 2019 17:34
%%%-------------------------------------------------------------------
-module(flowMeterTyp_tests).
-author("roeland").

-include_lib("eunit/include/eunit.hrl").

%% API
-export([]).

setup() ->
  meck:new(survivor2),
  meck:expect(survivor2, entry,
    fun(_) ->
      flowMeterTyp_created
    end),
  {ok, FlowMeterTypPid} = flowMeterTyp:create(),
  register(flowMeterTyp, FlowMeterTypPid).

cleanup(_) ->
  unregister(flowMeterTyp),
  meck:unload().

flowMeterTyp_test_() ->
  {setup,
    fun setup/0,
    fun cleanup/1,
    [{inorder,
      [fun test_create/0,
        fun test_initial_state/0,
        fun test_measure_flow/0,
        fun test_isOn/0]}]}.

test_create() ->
  ?assert(erlang:is_process_alive(whereis(flowMeterTyp))).

test_initial_state() ->
  meck:new(resource_instance),
  meck:expect(resource_instance, list_locations,
    fun(_) ->
      {ok, [chamber]}
    end),
  meck:new(location),
  meck:expect(location, get_Visitor,
    fun(_) ->
      {ok, fluidum}
    end),

  {ok, ActualState} = msg:get(whereis(flowMeterTyp), initial_state, [meterInstPid, [resInstPid, realWorldCmd]]),
  ExpectedState = #{meterInst => meterInstPid, resInst => resInstPid,
    fluidum => fluidum, rw_cmd => realWorldCmd},
  ?assertEqual(ActualState, ExpectedState).

test_measure_flow() ->
  {ok, Ok} = msg:get(whereis(flowMeterTyp), measure_flow, #{meterInst => meterInst, resInst => resInst,
    fluidum => fluidum, rw_cmd => fun() ->io:format("RealWrldCmdFn ~n")end}),

  ?assertEqual(ok, Ok).

%%test_estimate_flow() ->
%%  {ok, FlowMeterPid} = flowMeterTyp:create(),
%%
%%  meck:new(fluidumInst),
%%  meck:expect(fluidumInst, get_resource_circuit,
%%    fun(_) ->
%%      #{connectorIn1 => processed,connectorOut1 => processed,connectorIn2 => processed, connectorOut2 => processed}
%%    end),
%%
%%  {ok, State} = msg:get(FlowMeterPid, estimate_flow, #{meterInst => meterInst, resInst => resInst,
%%    fluidum => fluidum, rw_cmd => fun() ->io:format("RealWrldCmdFn ~n")end}),
%%
%%  ?assertEqual([kamer], State).

test_isOn() ->
  {ok, Off} = msg:get(whereis(flowMeterTyp), isOn, #{meterInst => meterInst, resInst => resInst,
    fluidum => fluidum, rw_cmd => fun() ->io:format("RealWrldCmdFn ~n")end, on_or_off => off}),
  ?assertEqual(off, Off),
  {ok, On} = msg:get(whereis(flowMeterTyp), isOn, #{meterInst => meterInst, resInst => resInst,
    fluidum => fluidum, rw_cmd => fun() ->io:format("RealWrldCmdFn ~n")end, on_or_off => on}),
  ?assertEqual(on, On).