%%%-------------------------------------------------------------------
%%% @author roeland
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Jan 2019 11:52
%%%-------------------------------------------------------------------
-module(pumpTyp_tests).
-author("roeland").

-include_lib("eunit/include/eunit.hrl").

%% API
-export([]).

%TODO: test create/init
setup() ->
  meck:new(survivor2),
  meck:expect(survivor2, entry,
    fun(_) ->
      pumpTyp_created
    end),
  {_,PumpTyp} = pumpTyp:create(),
  register(pumpTyp, PumpTyp).

cleanup(_) ->
  unregister(pumpTyp),
  meck:unload().

pipeTyp_test_() ->
  {setup,
    fun setup/0,
    fun cleanup/1,
    [{inorder,
      [fun test_create/0,
        fun test_initial_state/0,
        fun test_switchOff/0,
        fun test_switchOn/0,
        fun test_switchIsOn/0,
        fun test_flow_influence/0]}]}.

test_create() ->
  ?assert(erlang:is_process_alive(whereis(pumpTyp))).

test_initial_state() ->
  {ok, State} = msg:get(whereis(pumpTyp), initial_state, [resInstPid, [pipeInstPid, realWorldCmdFn]]),
  ?assertEqual(maps:get(on_or_off, State), off),
  ?assertEqual(maps:get(rw_cmd, State), realWorldCmdFn),
  ?assertEqual(maps:get(resInst, State), resInstPid),
  ?assertEqual(maps:get(pipeInst, State), pipeInstPid).

test_switchOff() ->
  {ok,State} = msg:get(whereis(pumpTyp), switchOff, #{resInst => resInst, pipeInst => pipeInst,
    rw_cmd => fun(X)->X end, on_or_off => on}),

  ?assertEqual(maps:get(on_or_off, State), off).

test_switchOn() ->
  {ok,State} = msg:get(whereis(pumpTyp), switchOn, #{resInst => resInst, pipeInst => pipeInst,
    rw_cmd => fun(X)->X end, on_or_off => off}),

  ?assertEqual(maps:get(on_or_off, State), on).

test_switchIsOn() ->
  {ok,IsOff} = msg:get(whereis(pumpTyp), isOn, #{resInst => resInst, pipeInst => pipeInst,
    rw_cmd => fun(X)->X end, on_or_off => off}),

  ?assertEqual(IsOff, off),

  {ok,IsOn} = msg:get(whereis(pumpTyp), isOn, #{resInst => resInst, pipeInst => pipeInst,
    rw_cmd => fun(X)->X end, on_or_off => on}),

  ?assertEqual(IsOn, on).

test_flow_influence() ->
  {ok,FunctionOn} = msg:get(whereis(pumpTyp), flow_influence, #{resInst => resInst, pipeInst => pipeInst,
    rw_cmd => fun(X)->X end, on_or_off => on}),
  ?assertNotEqual([],FunctionOn),

  {ok,FunctionOff} = msg:get(whereis(pumpTyp), flow_influence, #{resInst => resInst, pipeInst => pipeInst,
    rw_cmd => fun(X)->X end, on_or_off => off}),
  ?assertNotEqual([],FunctionOff),
  ?assertNotEqual(FunctionOff, FunctionOn).
