%%%-------------------------------------------------------------------
%%% @author roeland
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Jan 2019 17:21
%%%-------------------------------------------------------------------
-module(heatExchangerInst_tests).
-author("roeland").

-include_lib("eunit/include/eunit.hrl").

%% API
-export([]).

setup() ->
  meck:new(survivor2),
  meck:expect(survivor2, entry,
    fun(_) ->
      heatExchangerTyp_created
    end),
  {ok,HeatExchangerTyp} = heatExchangerTyp:create(),
  register(heatExchangerTyp, HeatExchangerTyp),

  {ok,HeatExchangerInst} = heatExchangerInst:create(self(), HeatExchangerTyp, pipeInst_Pid, hE_link_spec),
  register(heatExchangerInst, HeatExchangerInst).

cleanup(_) ->
  unregister(heatExchangerTyp),
  unregister(heatExchangerInst),
  meck:unload().

heatExchangerInst_test_() ->
  {setup,
    fun setup/0,
    fun cleanup/1,
    [{inorder,
      [fun test_create/0,
        fun test_get_type/0,
        fun test_get_temp_influence/0]}]}.

test_create() ->
  ?assert(erlang:is_process_alive(whereis(heatExchangerInst))).

test_get_type() ->
  {ok, Type} = msg:get(whereis(heatExchangerInst), get_type),
  ?assertEqual(whereis(heatExchangerTyp), Type).

test_get_temp_influence() ->
  meck:new(heatExchangeLink),
  meck:expect(heatExchangeLink, get_temp_influence,
    fun(_) ->
      temp_influence
    end),
  {ok, TempInfluence} = msg:get(whereis(heatExchangerInst), get_temp_influence),
  ?assertEqual(temp_influence, TempInfluence).
