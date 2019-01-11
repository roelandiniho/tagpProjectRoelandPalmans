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

initial_state_test() ->
  {_,PumpTyp} = pumpTyp:create(),

  {ok, State} = msg:get(PumpTyp, initial_state, [resInstPid, [pipeInstPid, realWorldCmdFn]]),
  ?assertEqual(maps:get(on_or_off, State), off),
  ?assertEqual(maps:get(rw_cmd, State), realWorldCmdFn),
  ?assertEqual(maps:get(resInst, State), resInstPid),
  ?assertEqual(maps:get(pipeInst, State), pipeInstPid).

switchOff_test() ->
  {_,PumpTyp} = pumpTyp:create(),

  {ok,State} = msg:get(PumpTyp, switchOff, #{resInst => resInst, pipeInst => pipeInst,
    rw_cmd => fun(X)->X end, on_or_off => on}),

  ?assertEqual(maps:get(on_or_off, State), off).

switchOn_test() ->
  {_,PumpTyp} = pumpTyp:create(),

  {ok,State} = msg:get(PumpTyp, switchOn, #{resInst => resInst, pipeInst => pipeInst,
    rw_cmd => fun(X)->X end, on_or_off => off}),

  ?assertEqual(maps:get(on_or_off, State), on).

switchIsOn_test() ->
  {_,PumpTyp} = pumpTyp:create(),

  {ok,IsOff} = msg:get(PumpTyp, isOn, #{resInst => resInst, pipeInst => pipeInst,
    rw_cmd => fun(X)->X end, on_or_off => off}),

  ?assertEqual(IsOff, off),

  {ok,IsOn} = msg:get(PumpTyp, isOn, #{resInst => resInst, pipeInst => pipeInst,
    rw_cmd => fun(X)->X end, on_or_off => on}),

  ?assertEqual(IsOn, on).

flow_influence_test() ->
  {_,PumpTyp} = pumpTyp:create(),

  {ok,FunctionOn} = msg:get(PumpTyp, flow_influence, #{resInst => resInst, pipeInst => pipeInst,
    rw_cmd => fun(X)->X end, on_or_off => on}),
  ?assertNotEqual([],FunctionOn),

  {ok,FunctionOff} = msg:get(PumpTyp, flow_influence, #{resInst => resInst, pipeInst => pipeInst,
    rw_cmd => fun(X)->X end, on_or_off => off}),
  ?assertNotEqual([],FunctionOff),
  ?assertNotEqual(FunctionOff, FunctionOn).
