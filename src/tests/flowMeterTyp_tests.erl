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

create_test() ->
  meck:new(survivor2),
  meck:expect(survivor2, entry,
    fun(_) ->
      pipeTyp_created
    end),
  {ok, FlowMeterPid} = flowMeterTyp:create(),
  ?assert(erlang:is_process_alive(FlowMeterPid)).


initial_state_test() ->
  {ok, FlowMeterPid} = flowMeterTyp:create(),

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

  {ok, ActualState} = msg:get(FlowMeterPid, initial_state, [meterInstPid, [resInstPid, realWorldCmd]]),
  ExpectedState = #{meterInst => meterInstPid, resInst => resInstPid,
    fluidum => fluidum, rw_cmd => realWorldCmd},
  ?assertEqual(ActualState, ExpectedState).

measure_flow_test() ->
  {ok, FlowMeterPid} = flowMeterTyp:create(),

  {ok, Ok} = msg:get(FlowMeterPid, measure_flow, #{meterInst => meterInst, resInst => resInst,
    fluidum => fluidum, rw_cmd => fun() ->io:format("RealWrldCmdFn ~n")end}),

  ?assertEqual(ok, Ok).

estimate_flow_test() ->
  {ok, FlowMeterPid} = flowMeterTyp:create(),

  meck:new(fluidumInst),
  meck:expect(fluidumInst, get_resource_circuit,
    fun(_) ->
      #{connectorIn1 => processed,connectorOut1 => processed,connectorIn2 => processed, connectorOut2 => processed}
    end),

  {ok, State} = msg:get(FlowMeterPid, estimate_flow, #{meterInst => meterInst, resInst => resInst,
    fluidum => fluidum, rw_cmd => fun() ->io:format("RealWrldCmdFn ~n")end}),

  ?assertEqual([kamer], State).

ioOn_test() ->
  {ok, FlowMeterPid} = flowMeterTyp:create(),
  {ok, Off} = msg:get(FlowMeterPid, isOn, #{meterInst => meterInst, resInst => resInst,
    fluidum => fluidum, rw_cmd => fun() ->io:format("RealWrldCmdFn ~n")end, on_or_off => off}),
  ?assertEqual(off, Off),
  {ok, On} = msg:get(FlowMeterPid, isOn, #{meterInst => meterInst, resInst => resInst,
    fluidum => fluidum, rw_cmd => fun() ->io:format("RealWrldCmdFn ~n")end, on_or_off => on}),
  ?assertEqual(on, On).