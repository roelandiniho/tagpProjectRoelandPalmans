%%%-------------------------------------------------------------------
%%% @author roeland
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Jan 2019 22:24
%%%-------------------------------------------------------------------
-module(resource_type_tests).
-author("roeland").

-include_lib("eunit/include/eunit.hrl").

%% API
-export([]).

setup() ->
  meck:unload(),
  meck:new(survivor2).

cleanup(_) ->
  meck:unload().

resource_type_test_() ->
  {setup,
    fun setup/0,
    fun cleanup/1,
    [{inorder,
      [fun test_create_pipeTyp/0]}]}.

test_create_pipeTyp() ->
  test_create_resource(pipeTyp, []),
  test_create_resource(pumpTyp, []),
  test_create_resource(fluidumTyp, []),
  test_create_resource(flowMeterTyp, []),
  test_create_resource(heatExchangerTyp, []).


test_create_resource(Selector, ParameterList) ->
  meck:expect(survivor2, entry,
    fun(_) ->
      Selector
    end),

  ResourceType = resource_type:create(Selector, ParameterList),
  ResourceTypePid = getResourceTypPid(ResourceType),
  (whereis(Selector) =:= undefined) orelse unregister(Selector),
  register(Selector, ResourceTypePid),
  ?assert(erlang:is_process_alive(whereis(Selector))),
  unregister(Selector).

getResourceTypPid({ok,ResourceType}) -> ResourceType;
getResourceTypPid(ResourceType) -> ResourceType.