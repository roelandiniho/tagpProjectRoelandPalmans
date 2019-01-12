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

setup() ->
  meck:new(survivor2),
  meck:expect(survivor2, entry,
    fun(_) ->
      fluidTyp_created
    end),
  FluidumTyp = fluidumTyp:create(),
  register(fluidumTyp, FluidumTyp).

cleanup(_) ->
  unregister(fluidumTyp),
  meck:unload().

fluidumTyp_test_() ->
  {setup,
    fun setup/0,
    fun cleanup/1,
    [{inorder,
      [fun test_create/0,
%%        fun test_initial_state/0,
        fun test_connection_list/0,
        fun test_location_list/0
%%        fun test_resource_circuit/0,
%%        fun test_get_resource_circuit/0,
%%        fun test_discover_circuit/0
    ]}]}.

test_create() ->
  ?assert(erlang:is_process_alive(whereis(fluidumTyp))).

%%test_initial_state() ->
%%  {ok, State} = msg:get(FluidumTyp, initial_state, [resInstPid, [root_connecterPid, typeOptions]]),
%%  ?assertEqual(maps:get(on_or_off, State), off),
%%  ?assertEqual(maps:get(rw_cmd, State), realWorldCmdFn),
%%  ?assertEqual(maps:get(resInst, State), resInstPid),
%%  ?assertEqual(maps:get(pipeInst, State), pipeInstPid).

test_connection_list() ->
  ?assertEqual({ok,[]}, msg:get(whereis(fluidumTyp), connections_list, empty)).

test_location_list() ->
  ?assertEqual({ok,[]}, msg:get(whereis(fluidumTyp), locations_list, empty)).

%%test_resource_circuit() ->
%%  {ok,Function} = msg:get(FluidumTyp, flow_influence, empty),
%%
%%  ?assertNotEqual([],Function).

%%test_get_resource_circuit() ->
%%  fluidumTyp:get_resource_circuit(FluidumTyp, #{resInst, circuit, typeOptions}).

%%test_discover_circuit() ->
%%  Circuit = fluidumTyp:discover_circuit(empty),
%%  ?assertEqual([],Circuit).