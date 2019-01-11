%%%-------------------------------------------------------------------
%%% @author roeland
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Jan 2019 15:46
%%%-------------------------------------------------------------------
-module(fluidumTyp_tests).
-author("roeland").

-include_lib("eunit/include/eunit.hrl").

%% API
-export([]).
create_test() ->
  FluidumTyp = fluidumTyp:create(),
  ?assertNotEqual([], FluidumTyp).

%%initial_state_test() ->
%%  FluidumTyp = fluidumTyp:create(),
%%
%%  {ok, State} = msg:get(FluidumTyp, initial_state, [resInstPid, [root_connecterPid, typeOptions]]),
%%  ?assertEqual(maps:get(on_or_off, State), off),
%%  ?assertEqual(maps:get(rw_cmd, State), realWorldCmdFn),
%%  ?assertEqual(maps:get(resInst, State), resInstPid),
%%  ?assertEqual(maps:get(pipeInst, State), pipeInstPid).

connection_list_test() ->
  FluidumTyp = fluidumTyp:create(),

  ?assertEqual({ok,[]}, msg:get(FluidumTyp, connections_list, empty)).

location_list_test() ->
  FluidumTyp = fluidumTyp:create(),

  ?assertEqual({ok,[]}, msg:get(FluidumTyp, locations_list, empty)).

%%resource_circuit_test() ->
%%  FluidumTyp = fluidumTyp:create(),
%%
%%  {ok,Function} = msg:get(FluidumTyp, flow_influence, empty),
%%
%%  ?assertNotEqual([],Function).

%%get_resource_circuit_test() ->
%%  FluidumTyp = fluidumTyp:create(),
%%  fluidumTyp:get_resource_circuit(FluidumTyp, #{resInst, circuit, typeOptions}).

%%discover_circuit_test() ->
%%  Circuit = fluidumTyp:discover_circuit(empty),
%%  ?assertEqual([],Circuit).