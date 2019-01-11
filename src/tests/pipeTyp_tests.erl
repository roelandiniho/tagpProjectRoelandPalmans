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

%%setup() ->
%%  meck:new(survivor2),
%%  meck:expect(survivor2, entry,
%%    fun(_) ->
%%      pipeTyp_created
%%    end),
%%  {ok,PipeTyp}=pipeTyp:create(),
%%  register(pipeTyp, PipeTyp),
%%  PipeTyp,
%%  ?debugMsg("setup").
%%
%%cleanup() ->
%%  unregister(pipeTyp),
%%  meck:unload(),
%%  ?debugMsg("cleanup").
%%
%%pipeTyp_test_() ->
%%  {setup,
%%    fun setup/0,
%%    fun cleanup/0,
%%    [?_assert(true)]
%%  }.




create_test() ->
  meck:new(survivor2),
  meck:expect(survivor2, entry,
    fun(_) ->
      pipeTyp_created
    end),
  {ok,PipeTyp} = pipeTyp:create(),
  ?assert(erlang:is_process_alive(PipeTyp)).

initial_state_test() ->
  {ok,PipeTyp} = pipeTyp:create(),
  meck:new(location),
  meck:expect(location, create, fun(_, emptySpace) -> locationPid end),

  meck:new(connector),
  meck:expect(connector, create, fun(_, simplePipe) -> connectorPid end),

  {ok, State} = msg:get(PipeTyp, initial_state, [resInstPid, typeOptions]),
  ?assertEqual([connectorPid,connectorPid], maps:get(cList, State)),
  ?assertEqual([locationPid], maps:get(chambers, State)),
  ?assertEqual(maps:get(resInst, State), resInstPid),
  ?assertEqual(maps:get(typeOptions, State), typeOptions).

connection_list_test() ->
  {ok,PipeTyp} = pipeTyp:create(),

  {ok,Clist} = msg:get(PipeTyp, connections_list, #{cList => [cIn,cOut],
    chambers => [kamer],
    resInst => resInst,
    typeOptions => []}),

  ?assertEqual([cIn,cOut], Clist).

location_list_test() ->
  {ok,PipeTyp} = pipeTyp:create(),

  {ok,L_List} = msg:get(PipeTyp, locations_list, #{cList => [cIn,cOut],
    chambers => [kamer],
    resInst => resInst,
    typeOptions => []}),

  ?assertEqual([kamer], L_List).

flow_influence_test() ->
  {ok,PipeTyp} = pipeTyp:create(),

  {ok,Function} = msg:get(PipeTyp, flow_influence, state),

  ?assertNotEqual([],Function).
