%%%-------------------------------------------------------------------
%%% @author roeland
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Jan 2019 21:57
%%%-------------------------------------------------------------------
-module(heatExchangerTyp_tests).
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
  register(heatExchangerTyp, HeatExchangerTyp).

cleanup(_) ->
  unregister(heatExchangerTyp),
  meck:unload().

heatExchangerTyp_test_() ->
  {setup,
    fun setup/0,
    fun cleanup/1,
    [{inorder,
      [fun test_create/0,
        fun test_initial_state/0]}]}.

test_create() ->
  ?assert(erlang:is_process_alive(whereis(heatExchangerTyp))).

test_initial_state() ->
  {ok, State} = msg:get(whereis(heatExchangerTyp), initial_state, [resInstPid, pipeInst_Pid]),
  ?assertEqual(maps:get(resInst, State), resInstPid),
  ?assertEqual(maps:get(pipeInst, State), pipeInst_Pid).