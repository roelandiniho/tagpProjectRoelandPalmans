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
  %Test starts survivor2
  io:format("survivor2: "),
  eunit:test(survivor2),

  io:format("pipeTyp: "),
  eunit:test(pipeTyp),
  io:format("location: "),
  eunit:test(location),
  io:format("connector: "),
  eunit:test(connector),
  io:format("pumpTyp: "),
  eunit:test(pumpTyp),
  io:format("fluidumTyp: "),
  eunit:test(fluidumTyp),
  io:format("flowMeterTyp: "),
  eunit:test(flowMeterTyp).