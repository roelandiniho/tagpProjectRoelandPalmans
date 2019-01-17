%%%-------------------------------------------------------------------
%%% @author roeland
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Jan 2019 18:02
%%%-------------------------------------------------------------------
-module(runTests).
-author("roeland").

%% API
-export([runTests/0]).

runTests() ->
  io:format("survivor2: "),
  eunit:test(survivor2),

  io:format("pipeTyp: "),
  eunit:test(pipeTyp),
  io:format("pipeInst: "),
  eunit:test(pipeInst),
  io:format("location: "),
  eunit:test(location),
  io:format("connector: "),
  eunit:test(connector),
  io:format("resource_type: "),
  eunit:test(resource_type),
  io:format("pumpTyp: "),
  eunit:test(pumpTyp),
%%  io:format("pumpInst: "),
%%  eunit:test(pumpInst),
  io:format("fluidumTyp: "),
  eunit:test(fluidumTyp),
  io:format("fluidumInst: "),
  eunit:test(fluidumInst),
  io:format("flowMeterTyp: "),
  eunit:test(flowMeterTyp),
  io:format("flowMeterInst: "),
  eunit:test(flowMeterInst),
  io:format("heatExchangerTyp: "),
  eunit:test(heatExchangerTyp),
%%  io:format("heatExchangerInst: "),
%%  eunit:test(heatExchangerInst),
  io:format("heatExchangeLink: "),
  eunit:test(heatExchangeLink).