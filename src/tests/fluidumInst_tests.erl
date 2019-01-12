%%%-------------------------------------------------------------------
%%% @author roeland
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Jan 2019 16:30
%%%-------------------------------------------------------------------
-module(fluidumInst_tests).
-author("roeland").

-include_lib("eunit/include/eunit.hrl").

%% API
-export([]).


%TODO: fixt tests
%%setup() ->
%%  meck:new(survivor2),
%%  meck:expect(survivor2, entry,
%%    fun({_,_}) ->
%%      fluidInst_created
%%    end),
%%  meck:expect(survivor2, entry,
%%    fun(_) ->
%%      fluidTyp_created
%%    end),
%%
%%  FluidumTyp = fluidumTyp:create(),
%%  register(fluidumTyp, FluidumTyp),
%%
%%  meck:new(fluidumTyp),
%%  meck:expect(fluidumTyp, discover_circuit,
%%    fun(_) ->
%%      {ok, circuit}
%%    end),
%%
%%  {ok,FluidumInst} = fluidumInst:create(rootConnectorPid, FluidumTyp),
%%  register(fluidumInst, FluidumInst).
%%
%%cleanup(_) ->
%%  unregister(fluidumInst),
%%  meck:unload().
%%
%%fluidumInst_test_() ->
%%  {setup,
%%    fun setup/0,
%%    fun cleanup/1,
%%    [{inorder,
%%      [fun test_create/0,
%%        fun test_get_locations/0
%%        fun test_get_type/0,
%%        fun test_get_resource_circuit/0
%%      ]}]}.
%%
%%test_create() ->
%%  ?assert(erlang:is_process_alive(whereis(fluidumInst))).
%%
%%test_get_locations() ->
%%  meck:new(resource_type),
%%  meck:expect(resource_type, get_locations_list,
%%    fun(_,_) ->
%%      {ok, [chamber]}
%%    end),
%%  List = msg:get(whereis(fluidumInst), get_locations),
%%  ?assertEqual([chamber], List).

%%test_get_type() ->
%%  {ok,ResTyp_Pid} = msg:get(whereis(fluidumInst), get_type),
%%  ?assertEqual(whereis(fluidumTyp), ResTyp_Pid).
%%
%%test_get_resource_circuit() ->
%%  meck:new(fluidumTyp),
%%  meck:expect(fluidumTyp, get_resource_circuit,
%%  fun(_,_) ->
%%    resourceCircuit
%%  end),
%%  {ok,C} = msg:get(whereis(fluidumInst), get_locations),
%%  ?assertEqual(resourceCircuit, C).