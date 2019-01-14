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

-spec test() -> 'ok'.
test() ->
  survivor2:start(),

  %Input the circuit in order
  %For pump, heatExchanger and flowMeter, extra variable: function, has to be added
  SimpleCircuit = [
    pipe,
    {pump, fun(X) -> X end},
    {heatExchanger, 5},
    pipe,
    {flowMeter, fun() -> io:format("RealWrldCmdFn ~n") end},
    pipe],

  create_simple_circuit(SimpleCircuit),

  {_,[Pipe1In,_]} = resource_instance:list_connectors(pipeInst1),
  {_,FluidumInst1} = resource_instance:create(fluidumInst, [Pipe1In, fluidumTyp]),
    io:format("FluidumInst1 : ~p. ~n",[FluidumInst1]).

%%  %Create fluidum
%%  {_,[Pipe1In,_]}= resource_instance:list_connectors(PipeInst1),
%%  {_,FluidumInst1} = resource_instance:create(fluidumInst, [Pipe1In, FluidumTyp1]),
%%    io:format("FluidumInst1 : ~p. ~n",[FluidumInst1]),
%%
%%  {ok, [L | _ ] } = resource_instance:list_locations(PipeInst2),
%%  location:arrival(L, FluidumInst1),




%Calls the different functions to create the circuit
create_simple_circuit([]) -> ok;
create_simple_circuit([H|T]) ->
  create_types(),
  create_pipes(length([H|T])),
  create_parts([H|T]),
  connectPipes(length([H|T])).

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
  {_,PipeInst} = resource_instance:create(pipeInst, [host, pipeTyp]),
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
      {_,PumpInstPid} = resource_instance:create(pumpInst, [host, pumpTyp, create_atom(pipeInst, Counter), RealWorldCmdFn]),
      register(checkAvailablePid(pumpInst), PumpInstPid);
    {heatExchanger, HE_link_spec} ->
      {_,HeatExchangerInst1} = resource_instance:create(heatExchangerInst, [host, heatExchangerTyp, create_atom(pipeInst, Counter), HE_link_spec]),
      register(checkAvailablePid(heatExchangerInst), HeatExchangerInst1);
    {flowMeter, RealWorldCmdFn} ->
      {_,FlowMeterInst1} = resource_instance:create(flowMeterInst, [host, flowMeterTyp, create_atom(pipeInst, Counter), RealWorldCmdFn]),
      register(checkAvailablePid(flowMeterInst), FlowMeterInst1);
      Else ->
        create_atom(Else, non_existing)
  end,
  create_parts(T, Counter+1).

%Connects all the pipes by In and Out connectors
connectPipes(Number) -> connectPipes(Number, 1).
connectPipes(Number, Counter) when  Counter+1 =< Number ->
  {_,[_,PipeAOut]} = resource_instance:list_connectors(create_atom(pipeInst, Counter)),
  {_,[PipeBIn,PipeBOut]} = resource_instance:list_connectors(create_atom(pipeInst, Counter+1)),
  X = connector:connect(PipeAOut, PipeBIn),
  Y = connector:connect(PipeBIn, PipeAOut),
  connectPipes(Number, Counter+1, PipeBOut);
connectPipes(_, _) -> ok.
connectPipes(Number, Counter, PipeAOut) when  Counter+1 =< Number ->
  {_,[PipeBIn,PipeBOut]} = resource_instance:list_connectors(create_atom(pipeInst, Counter+1)),
  X = connector:connect(PipeAOut, PipeBIn),
  Y = connector:connect(PipeBIn, PipeAOut),
  connectPipes(Number, Counter+1, PipeBOut);
connectPipes(_, _, _) -> ok.

%Remove the 'ok' part from a ResourceType if there is one
getResourceTypPid({ok,ResourceType}) -> ResourceType;
getResourceTypPid(ResourceType) -> ResourceType.

%Generate an available Pid of a Typ
checkAvailablePid(Type) -> checkAvailablePid(Type, 1).
checkAvailablePid(Type, Counter) ->
  case whereis(create_atom(Type, Counter)) of
    undefined -> create_atom(Type, Counter);
    (_) -> checkAvailablePid(Type, Counter + 1)
  end.

%Create an atom given a Type and the number of that type
create_atom(Type, Counter) ->
  list_to_atom(lists:flatten(io_lib:format("~p~p", [Type, Counter]))).