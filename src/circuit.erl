%%%-------------------------------------------------------------------
%%% @author roeland
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Jan 2019 19:30
%%%-------------------------------------------------------------------
-module(circuit).
-author("roeland").

%% API
-export([create/0, pumpTurnOn/1, getTemperatureInfluence/2, estimateFlow/0, setup/1, create_random_circuit/0]).

-compile(export_all).


%Dialyzer: Create a PLT file :
%     dialyzer --build_plt --apps kernel stdlib erts mnesia eunit
% Add all beam files to the PLT:
%     dialyzer --add_to_plt path_to_beam/*.beam
%User the dialyzer:
%     dialyzer *.erl
%Typer: Generate annotate files:
%     typer --annotate *.erl

%Input the circuit in order
%For pump, heatExchanger and flowMeter, extra variable: function, has to be added

-spec create() -> {'ok',pid()}.
create() ->
  SimpleCircuit = [
    pipe,
    {pump, fun(X) -> X end},
    {pump, fun(X) -> X end},
   {heatExchanger, #{delta => 5}},
    {heatExchanger, #{delta => 9}},
    pipe,
    {flowMeter, fun() -> io:format("RealWrldCmdFn ~n") end}],
  {ok, spawn(?MODULE, setup, [SimpleCircuit])}.

create_random_circuit() ->
  SimpleCircuit = generate_random_circuit(),
  {ok, spawn(?MODULE, setup, [SimpleCircuit])}.

-spec setup(list()) -> no_return().
setup(SimpleCircuit) ->
  survivor2:start(),

  create_simple_circuit(SimpleCircuit),

  {ok,[Pipe1In,_]} = resource_instance:list_connectors(pipeInst1),%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  {ok,FluidumInst1} = resource_instance:create(fluidumInst, [Pipe1In, fluidumTyp]),
  register(checkAvailableProcessName(fluidumInst), FluidumInst1),
  fluidumInst:fluidum_arrives_at_locations(FluidumInst1),%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  timer:sleep(1000),
  flowMeterInst:updateCircuit(flowMeterInst1),
  loop().

%Simple loop
-spec loop() -> 'ok'.
loop() ->
  receive
    stop -> ok
  end.

%Turn on Pump
-spec pumpTurnOn(pid()) -> 'switchOn'.
pumpTurnOn(PumpInstId) ->
  pumpInst:switch_on(PumpInstId).

%Calculate temperature influence
-spec getTemperatureInfluence(HeatExhangerPid::pid(),InTemp::number()) -> no_return().
getTemperatureInfluence(HeatExhangerPid, InTemp) ->
  {ok, Flow} = estimateFlow(),
  heatExchangerInst:get_temp_difference(HeatExhangerPid, [InTemp, Flow]).%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Estimate the flow
-spec estimateFlow() -> no_return().
estimateFlow() ->
  flowMeterInst:estimate_flow(flowMeterInst1).%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Calls the different functions to create the circuit
-spec create_simple_circuit([any()]) -> 'ok'.
create_simple_circuit([]) -> ok;
create_simple_circuit([H|T]) ->
  create_types(),
  create_pipes(length([H|T])),
  connectPipes(length([H|T])),
  create_parts([H|T]).

%Creates all the different types
-spec create_types() -> 'ok'.
create_types() -> create_types([pipeTyp, pumpTyp, fluidumTyp, flowMeterTyp, heatExchangerTyp]).
-spec create_types([atom()]) -> 'ok'.
create_types([]) -> ok;
create_types([Type|T]) ->
  ProcessInfo = resource_type:create(Type, []),
  ResourceTypePid = getResourceTypPid(ProcessInfo),
  register(Type, ResourceTypePid),
  create_types(T).

%Creates the amount of pipes necessary for the circuit
-spec create_pipes(Number::number()) -> 'ok'.
create_pipes(Number) -> create_pipes(Number, 1).
-spec create_pipes(Number::number(),Counter::number()) -> 'ok'.
create_pipes(Number, Counter) when  Counter =< Number ->
  {_,PipeInst} = resource_instance:create(pipeInst, [self(), pipeTyp]),
  register(create_atom(pipeInst, Counter), PipeInst),
  create_pipes(Number, Counter +1);
create_pipes(_,_) -> ok.

%Create the requested parts connected to the pipes in the order that was requested
-spec create_parts([any()]) -> 'ok'.
create_parts(Parts) -> create_parts(Parts, 1).
-spec create_parts([any()],_) -> 'ok'.
create_parts([], _) -> ok;
create_parts([Part| T], Counter) ->
  case Part of
    pipe ->
      ok;
    {pump, RealWorldCmdFn} ->
      {_,PumpInstPid} = resource_instance:create(pumpInst, [self(), pumpTyp, create_atom(pipeInst, Counter), RealWorldCmdFn]),
      register(checkAvailableProcessName(pumpInst), PumpInstPid);
    {heatExchanger, HE_link_spec} ->
      {_,HeatExchangerInst1} = resource_instance:create(heatExchangerInst, [self(), heatExchangerTyp, create_atom(pipeInst, Counter), HE_link_spec]),
      register(checkAvailableProcessName(heatExchangerInst), HeatExchangerInst1);
    {flowMeter, RealWorldCmdFn} ->
      {_,FlowMeterInst1} = resource_instance:create(flowMeterInst, [self(), flowMeterTyp, create_atom(pipeInst, Counter), RealWorldCmdFn]),
      register(checkAvailableProcessName(flowMeterInst), FlowMeterInst1);
      Else ->
        create_atom(Else, non_existing)
  end,
  create_parts(T, Counter+1).

%Connects all the pipes by In and Out connectors
-spec connectPipes(Number::number()) -> 'ok'.
connectPipes(Number) -> connectPipes(Number, 1).
-spec connectPipes(Number:: number(),Counter::number()) -> 'ok'.
connectPipes(Number, Counter) when  Counter+1 =< Number ->
  {_,[Start,PipeAOut]} = resource_instance:list_connectors(create_atom(pipeInst, Counter)),%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  {_,[PipeBIn,PipeBOut]} = resource_instance:list_connectors(create_atom(pipeInst, Counter+1)),
  connectConnectors(PipeAOut, PipeBIn),
  connectPipes(Number, Counter+1, PipeBOut, Start);
connectPipes(_, _) -> ok.
-spec connectPipes(Number::number(),Counter::number(),PipeAOut::pid(),Start::pid()) -> {'connect',pid()}.
connectPipes(Number, Counter, PipeAOut, Start) when  Counter+1 =< Number ->
  {_,[PipeBIn,PipeBOut]} = resource_instance:list_connectors(create_atom(pipeInst, Counter+1)),%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  connectConnectors(PipeAOut, PipeBIn),
  connectPipes(Number, Counter+1, PipeBOut, Start);
connectPipes(_, _, PipeBOut, Start) ->
  connectConnectors(Start, PipeBOut).

%Connect an in connector with an out connector
-spec connectConnectors(In::pid(),Out::pid()) -> {'connect',pid()}.
connectConnectors(In, Out) ->
  connector:connect(In, Out),
  connector:connect(Out, In).

%Remove the 'ok' part from a ResourceType if there is one
-spec getResourceTypPid(_) -> any().
getResourceTypPid({ok,ResourceType}) -> ResourceType;
getResourceTypPid(ResourceType) -> ResourceType.

%Generate an available Pid of a Typ
-spec checkAvailableProcessName(_) -> atom().
checkAvailableProcessName(Type) -> checkAvailableProcessName(Type, 1).
-spec checkAvailableProcessName(_,Counter::number()) -> atom().
checkAvailableProcessName(Type, Counter) ->
  case whereis(create_atom(Type, Counter)) of
    undefined -> create_atom(Type, Counter);
    (_) -> checkAvailableProcessName(Type, Counter + 1)
  end.

%Create an atom given a Type and the number of that type
-spec create_atom(_,_) -> atom().
create_atom(Type, Counter) ->
  list_to_atom(lists:flatten(io_lib:format("~p~p", [Type, Counter]))).

%generate list with 1-10 pipes, 1-10 pumps, and 1-10 heatExchangers with a random temperature influence
generate_random_circuit() ->
  lists:append([
    [pipe || _ <- lists:seq(1, random:uniform(10))],
    [{pump, fun(X) -> X end} || _ <- lists:seq(1, random:uniform(10))],
    [{heatExchanger, #{delta => random:uniform(10)}} || _ <- lists:seq(1, random:uniform(10))],
    [{flowMeter, fun() -> io:format("RealWrldCmdFn ~n") end}]]).