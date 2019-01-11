%%%-------------------------------------------------------------------
%%% @author roeland
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Jan 2019 19:30
%%%-------------------------------------------------------------------
-module(testBench).
-author("roeland").

%% API
-export([test/0]).


%Dialyzer: Create a PLT file :
%     dialyzer --build_plt --apps kernel stdlib erts mnesia eunit
% Add all beam files to the PLT:
%     dialyzer --add_to_plt path_to_beam/*.beam
%User the dialyzer:
%     dialyzer *.erl
%Typer: Generate annotate files:
%     typer --annotate *.erl

-spec test() -> 'ok'.
test() ->
  survivor2:start(),

  %create 5 pipes
  {_,PipeTyp1} = resource_type:create(pipeTyp, []),
    io:format("PipeTyp1 : ~p. ~n",[PipeTyp1]),
  {_,PipeInst1} = resource_instance:create(pipeInst, [host, PipeTyp1]),
    io:format("PipeInst1 : ~p. ~n",[PipeInst1]),
  {_, PipeInst2} = resource_instance:create(pipeInst, [host, PipeTyp1]),
    io:format("PipeInst2 : ~p. ~n",[PipeInst2]),
  {_, PipeInst3} = resource_instance:create(pipeInst, [host, PipeTyp1]),
  io:format("PipeInst3 : ~p. ~n",[PipeInst3]),
  {_, PipeInst4} = resource_instance:create(pipeInst, [host, PipeTyp1]),
  io:format("PipeInst4 : ~p. ~n",[PipeInst4]),
  {_, PipeInst5} = resource_instance:create(pipeInst, [host, PipeTyp1]),
  io:format("PipeInst5 : ~p. ~n",[PipeInst5]),

  %Connect list of pipes
  connectPipes([PipeInst1, PipeInst2, PipeInst3, PipeInst4, PipeInst5]),

  %Create Pump over pipe 1
  {_,PumpTyp1} = resource_type:create(pumpTyp, []),
    io:format("PumpTyp1 : ~p. ~n",[PumpTyp1]),
  {_,PumpInst1} = resource_instance:create(pumpInst, [host, PumpTyp1, PipeInst1, fun(X) -> X end]),
    io:format("PumpInst1 : ~p. ~n",[PumpInst1]),



  %Create fluidum
  FluidumTyp1 = resource_type:create(fluidumTyp, []),
    io:format("FluidumTyp1 : ~p. ~n",[FluidumTyp1]),
  {_,[Pipe1In,_]}= resource_instance:list_connectors(PipeInst1),
  {_,FluidumInst1} = resource_instance:create(fluidumInst, [Pipe1In, FluidumTyp1]),
    io:format("FluidumInst1 : ~p. ~n",[FluidumInst1]),

  {ok, [L | _ ] } = resource_instance:list_locations(PipeInst2),
  location:arrival(L, FluidumInst1),

  %Create Flowmeter
  {_,FlowMeterTyp1} = resource_type:create(flowMeterTyp, []),
    io:format("FlowMeterTyp1 : ~p. ~n",[FlowMeterTyp1]),
  {_,FlowMeterInst1} = resource_instance:create(flowMeterInst, [host, FlowMeterTyp1, PipeInst2, fun() -> io:format("RealWrldCmdFn ~n") end]),
    io:format("FlowMeterInst1 : ~p. ~n",[FlowMeterInst1]),

  %Create HeatExchanger
  {_,HeatExchangerTyp1} = resource_type:create(heatExchangerTyp, []),
    io:format("HeatExchangerTyp1 : ~p. ~n",[HeatExchangerTyp1]),
  {_,HeatExchangerInst1} = resource_instance:create(heatExchangerInst, [host, HeatExchangerTyp1, PipeInst3, 5]),
    io:format("HeatExchangerInst1 : ~p. ~n",[HeatExchangerInst1]).

-spec connectPipes([pid()]) -> 'ok'.
connectPipes([PipeInstA, PipeInstB | Tail]) ->
  {_,[_,PipeAOut]}= resource_instance:list_connectors(PipeInstA),
  {_,[PipeBIn,PipeBOut]}= resource_instance:list_connectors(PipeInstB),
  X = connector:connect(PipeAOut, PipeBIn),
  connector:connect(PipeBIn, PipeAOut),
  connectPipes(Tail, PipeBOut).

-spec connectPipes([pid()],pid()) -> 'ok'.
connectPipes([], _) -> ok;
connectPipes([PipeInstB | Tail], PipeAOut) ->
  {_,[PipeBIn,PipeBOut]}= resource_instance:list_connectors(PipeInstB),
  connector:connect(PipeAOut, PipeBIn),
  connector:connect(PipeBIn, PipeAOut),
  connectPipes(Tail, PipeBOut).


