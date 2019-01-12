%%%-------------------------------------------------------------------
%%% @author roeland
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Jan 2019 15:19
%%%-------------------------------------------------------------------
-module(pipeInst_tests).
-author("roeland").

-include_lib("eunit/include/eunit.hrl").

%% API
-export([]).

setup() ->
  meck:new(survivor2),
  meck:expect(survivor2, entry,
    fun({_,_}) ->
      pipeInst_created
    end),
  meck:new(resource_type),
  meck:expect(resource_type, get_initial_state,
    fun(_,_,_) ->
      {ok, #{cList => [connectorIn,connectorOut],
        chambers => [chamber],
        resInst => pipeInst,typeOptions => []}}
    end),

  {ok,PipeInst} = pipeInst:create(host, pipeTypPid),
  register(pipeInst, PipeInst).

cleanup(_) ->
  unregister(pipeInst),
  meck:unload().

pipeInst_test_() ->
  {setup,
    fun setup/0,
    fun cleanup/1,
    [{inorder,
      [fun test_create/0,
        fun test_get_flow_influence/0,
        fun test_get_connectors/0,
        fun test_get_locations/0,
        fun test_get_type/0,
        fun test_get_ops/0,
        fun test_get_state/0]}]}.

test_create() ->
  ?assert(erlang:is_process_alive(whereis(pipeInst))).

test_get_flow_influence() -> ok.

test_get_connectors() ->
  meck:expect(resource_type, get_connections_list,
    fun(_,State) ->
      #{cList := C_List} = State,
      {ok,C_List}
    end),

  {ok,C_List} = msg:get(whereis(pipeInst), get_connectors),
  ?assertEqual([connectorIn,connectorOut], C_List).

test_get_locations() ->
  meck:expect(resource_type, get_locations_list,
    fun(_,State) ->
      #{chambers := L_List}= State,
      {ok,L_List}
    end),

  {ok,List} = msg:get(whereis(pipeInst), get_locations),
  ?assertEqual([chamber], List).

test_get_type() ->
  {ok,Type} = msg:get(whereis(pipeInst), get_type),
  ?assertEqual(pipeTypPid, Type).

test_get_ops() ->
  {ok,Ops} = msg:get(whereis(pipeInst), get_ops),
  ?assertEqual([], Ops).

test_get_state() ->
  {ok,State} = msg:get(whereis(pipeInst), get_state),
  ExpectedState =#{cList => [connectorIn,connectorOut],
    chambers => [chamber],
    resInst => pipeInst,typeOptions => []},
  ?assertEqual(ExpectedState, State).
