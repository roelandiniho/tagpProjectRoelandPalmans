%%%-------------------------------------------------------------------
%%% @author roeland
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Jan 2019 14:33
%%%-------------------------------------------------------------------
-module(survivor2_tests).
-author("roeland").

-include_lib("eunit/include/eunit.hrl").

%% API
-export([]).

setup() ->
  survivor2:start().

cleanup(_) ->
  unregister(survivor).

survivor2_test_() ->
  {setup,
    fun setup/0,
    fun cleanup/1,
    [{inorder,
      [fun test_start/0,
        fun test_restart/0,
        fun test_entry/0]}]}.


test_start() ->
  ?assert(erlang:is_process_alive(whereis(survivor))),
  ?assertNotEqual([], ets:info(logboek)).

test_restart() ->
  ?assert(survivor2:start()),
  ?assertNotEqual([], ets:info(logboek)).

test_entry() ->
  survivor2:entry("entry_test"),
  [{{{_,_,_},_},EntryData}] = ets:match_object(logboek, {'$0', '$1'}),
  ?assertEqual("entry_test", EntryData).

