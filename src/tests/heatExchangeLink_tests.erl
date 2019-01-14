%%%-------------------------------------------------------------------
%%% @author roeland
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Jan 2019 17:00
%%%-------------------------------------------------------------------
-module(heatExchangeLink_tests).
-author("roeland").

-include_lib("eunit/include/eunit.hrl").
-define(FORALL(X,RawType,Prop), proper:forall(RawType,fun(X) -> Prop end)).

%% API
-export([]).

setup() -> ok.

cleanup(_) -> ok.

heatExchangeLink_test_() ->
  {setup,
    fun setup/0,
    fun cleanup/1,
    [{inorder,
      [fun test_different_temps/0]}]}.

test_different_temps() ->
  ?assertEqual(3+1/2,test_get_temp_influence(1,2,3)),
  ?assertEqual(4+2/3,test_get_temp_influence(2,3,4)),
  ?assertEqual(5+3/4,test_get_temp_influence(3,4,5)),
  ?assertEqual(6+4/5,test_get_temp_influence(4,5,6)),
  ?assertEqual(7+5/6,test_get_temp_influence(5,6,7)),
  ?assertEqual(8+6/7,test_get_temp_influence(6,7,8)).

test_get_temp_influence(Difference, Flow, InTemp) ->
  {ok, Function} = heatExchangeLink:get_temp_influence(#{delta => Difference}),
  {ok, Temp} = Function(Flow,InTemp),
  Temp.