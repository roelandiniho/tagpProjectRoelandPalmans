%%%-------------------------------------------------------------------
%%% @author roeland
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Jan 2019 15:53
%%%-------------------------------------------------------------------
-module(flowMeterInst_tests).
-author("roeland").

-include_lib("eunit/include/eunit.hrl").

%% API
-export([]).

setup() ->
  meck:new(survivor2),
  meck:expect(survivor2, entry,
    fun({_,_}) ->
      flowMeterInst_created
    end),
  meck:expect(survivor2, entry,
    fun(_) ->
      flowMeterTyp_created
    end),

  {ok, FlowMeterTypPid} = flowMeterTyp:create(),
  register(flowMeterTyp, FlowMeterTypPid),

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

  {ok,FlowMeterInst} = flowMeterInst:create(host, FlowMeterTypPid, resInst_Pid, realWorldCmdFn),
  register(flowMeterInst, FlowMeterInst).

cleanup(_) ->
  unregister(flowMeterTyp),
  unregister(flowMeterInst),
  meck:unload().

flowMeterInst_test_() ->
  {setup,
    fun setup/0,
    fun cleanup/1,
    [{inorder,
      [fun test_create/0,
%%        fun test_estimate_flow/0,
%%        fun test_measure_flow/0,
        fun test_get_type/0]}]}.

test_create() ->
  ?assert(erlang:is_process_alive(whereis(flowMeterInst))).

%%test_estimate_flow() ->
%%  meck:new(msg),
%%  meck:expect(msg, get,
%%    fun(flowMeterTypPid,estimate_flow, _) ->
%%      measured_flow
%%    end),
%%
%%  Measure_flow = msg:get(whereis(flowMeterInst), estimate_flow),
%%  ?assertEqual(measured_flow, Measure_flow).

%%test_measure_flow() ->
%%  ok.

test_get_type() ->
  {ok,FlowMeterTyp_Pid} = msg:get(whereis(flowMeterInst), get_type),
  ?assertEqual(whereis(flowMeterTyp), FlowMeterTyp_Pid).
