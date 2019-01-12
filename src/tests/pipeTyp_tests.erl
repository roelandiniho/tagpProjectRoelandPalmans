%%%-------------------------------------------------------------------
%%% @author roeland
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Jan 2019 17:01
%%%-------------------------------------------------------------------
-module(pipeTyp_tests).
-author("roeland").

-include_lib("eunit/include/eunit.hrl").

%% API
-export([]).

setup() ->
  meck:new(survivor2),
  meck:expect(survivor2, entry,
    fun(_) ->
      pipeTyp_created
    end),
  {ok,PipeTyp} = pipeTyp:create(),
  register(pipeTyp, PipeTyp).

cleanup(_) ->
  unregister(pipeTyp),
  meck:unload().

pipeTyp_test_() ->
  {setup,
    fun setup/0,
    fun cleanup/1,
    [{inorder,
      [fun test_create/0,
        fun test_initial_state/0,
        fun test_connection_list/0,
        fun test_location_list/0,
        fun test_flow_influence/0]}]}.

test_create() ->
  ?assert(erlang:is_process_alive(whereis(pipeTyp))).

test_initial_state() ->
  meck:new(location),
  meck:expect(location, create, fun(_, emptySpace) -> locationPid end),
  meck:new(connector),
  meck:expect(connector, create, fun(_, simplePipe) -> connectorPid end),

  {ok, State} = msg:get(whereis(pipeTyp), initial_state, [resInstPid, typeOptions]),
  ?assertEqual([connectorPid,connectorPid], maps:get(cList, State)),
  ?assertEqual([locationPid], maps:get(chambers, State)),
  ?assertEqual(maps:get(resInst, State), resInstPid),
  ?assertEqual(maps:get(typeOptions, State), typeOptions).

test_connection_list() ->
  {ok,Clist} = msg:get(whereis(pipeTyp), connections_list, #{cList => [cIn,cOut],
    chambers => [kamer],
    resInst => resInst,
    typeOptions => []}),

  ?assertEqual([cIn,cOut], Clist).

test_location_list() ->
  {ok,L_List} = msg:get(whereis(pipeTyp), locations_list, #{cList => [cIn,cOut],
    chambers => [kamer],
    resInst => resInst,
    typeOptions => []}),

  ?assertEqual([kamer], L_List).

test_flow_influence() ->
  {ok,Function} = msg:get(whereis(pipeTyp), flow_influence, state),

  ?assertNotEqual([],Function).
