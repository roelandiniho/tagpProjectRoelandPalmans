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
-export([]).

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
create() ->
%%  SimplePipes =[
%%    pipe,
%%    pipe,
%%    pipe,
%%    pipe,
%%    pipe
%%  ],
  SimpleCircuit = [
    pipe,
    {pump, fun(X) -> X end},
    {pump, fun(X) -> X end},
    pipe,
    pipe,
    {flowMeter, fun() -> io:format("RealWrldCmdFn ~n") end}],
  {ok, spawn(?MODULE, setup, [SimpleCircuit])}.

-spec setup(list()) -> 'ok'.
setup(SimpleCircuit) ->
  survivor2:start(),
  create_simple_circuit(SimpleCircuit),
  loop().

%Simple loop
loop() ->
  receive
    stop -> ok
  end.

pumpTurnOn(PumpInstId) ->
  pumpInst:switch_on(PumpInstId).

%Calls the different functions to create the circuit
create_simple_circuit([]) -> ok;
create_simple_circuit([H|T]) ->
  create_types(),
  create_pipes(length([H|T])),
  connectPipes(length([H|T])),
  {ok,[Pipe1In,_]} = resource_instance:list_connectors(pipeInst1),
  {ok,FluidumInst1} = resource_instance:create(fluidumInst, [Pipe1In, fluidumTyp]),
  register(checkAvailableProcessName(fluidumInst), FluidumInst1),
  create_parts([H|T]).

%Creates all the different types
create_types() -> create_types([pipeTyp, pumpTyp, fluidumTyp, flowMeterTyp, heatExchangerTyp]).
create_types([]) -> ok;
create_types([Type|T]) ->
  ProcessInfo = resource_type:create(Type, []),
  ResourceTypePid = getResourceTypPid(ProcessInfo),
  register(Type, ResourceTypePid),
  create_types(T).

%Creates the amount of pipes necessary for the circuit
create_pipes(Number) -> create_pipes(Number, 1).
create_pipes(Number, Counter) when  Counter =< Number ->
  {_,PipeInst} = resource_instance:create(pipeInst, [self(), pipeTyp]),
  io:format("PipeInst ~p ~n",[PipeInst]),
  register(create_atom(pipeInst, Counter), PipeInst),
  create_pipes(Number, Counter +1);
create_pipes(_,_) -> ok.

%Create the requested parts connected to the pipes in the order that was requested
create_parts(Parts) -> create_parts(Parts, 1).
create_parts([], _) -> ok;
create_parts([Part| T], Counter) ->
  case Part of
    pipe ->
      ok;
    {pump, RealWorldCmdFn} ->
      {_,PumpInstPid} = resource_instance:create(pumpInst, [self(), pumpTyp, create_atom(pipeInst, Counter), RealWorldCmdFn]),
      io:format("PumpInstPid ~p ~n",[PumpInstPid]),
      register(checkAvailableProcessName(pumpInst), PumpInstPid),
      pumpInst:switch_on(PumpInstPid);
    {heatExchanger, HE_link_spec} ->
      {_,HeatExchangerInst1} = resource_instance:create(heatExchangerInst, [self(), heatExchangerTyp, create_atom(pipeInst, Counter), HE_link_spec]),
      io:format("HeatExchangerInst1 ~p ~n",[HeatExchangerInst1]),
      register(checkAvailableProcessName(heatExchangerInst), HeatExchangerInst1);
    {flowMeter, RealWorldCmdFn} ->
      {ok, [Location_Pid]} = resource_instance:list_locations(create_atom(pipeInst, Counter)),
      Visitor_Pid = whereis(fluidumInst1),
      location:arrival(Location_Pid, Visitor_Pid),
      {_,FlowMeterInst1} = resource_instance:create(flowMeterInst, [self(), flowMeterTyp, create_atom(pipeInst, Counter), RealWorldCmdFn]),
      io:format("FlowMeterInst1 ~p ~n",[FlowMeterInst1]),
      register(checkAvailableProcessName(flowMeterInst), FlowMeterInst1);
      Else ->
        create_atom(Else, non_existing)
  end,
  create_parts(T, Counter+1).

%Connects all the pipes by In and Out connectors
connectPipes(Number) -> connectPipes(Number, 1).
connectPipes(Number, Counter) when  Counter+1 =< Number ->
  {_,[Start,PipeAOut]} = resource_instance:list_connectors(create_atom(pipeInst, Counter)),
  {_,[PipeBIn,PipeBOut]} = resource_instance:list_connectors(create_atom(pipeInst, Counter+1)),
  connectConnectors(PipeAOut, PipeBIn),
  connectPipes(Number, Counter+1, PipeBOut, Start);
connectPipes(_, _) -> ok.
connectPipes(Number, Counter, PipeAOut, Start) when  Counter+1 =< Number ->
  {_,[PipeBIn,PipeBOut]} = resource_instance:list_connectors(create_atom(pipeInst, Counter+1)),
  connectConnectors(PipeAOut, PipeBIn),
  connectPipes(Number, Counter+1, PipeBOut, Start);
connectPipes(_, _, PipeBOut, Start) ->
  connectConnectors(Start, PipeBOut).

%Connect an in connector with an out connector
connectConnectors(In, Out) ->
  connector:connect(In, Out),
  connector:connect(Out, In).

%Remove the 'ok' part from a ResourceType if there is one
getResourceTypPid({ok,ResourceType}) -> ResourceType;
getResourceTypPid(ResourceType) -> ResourceType.

%Generate an available Pid of a Typ
checkAvailableProcessName(Type) -> checkAvailableProcessName(Type, 1).
checkAvailableProcessName(Type, Counter) ->
  case whereis(create_atom(Type, Counter)) of
    undefined -> create_atom(Type, Counter);
    (_) -> checkAvailableProcessName(Type, Counter + 1)
  end.

%Create an atom given a Type and the number of that type
create_atom(Type, Counter) ->
  list_to_atom(lists:flatten(io_lib:format("~p~p", [Type, Counter]))).