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

start_test() ->
  ?assert(survivor2:start()),
  ?assertNotEqual([], ets:info(logboek)).

restart_test() ->
  ?assert(survivor2:start()),
  ?assertNotEqual([], ets:info(logboek)).

entry_test() ->
  survivor2:entry("entry_test"),
  [{{{_,_,_},_},EntryData}] = ets:match_object(logboek, {'$0', '$1'}),
  ?assertEqual("entry_test", EntryData).

