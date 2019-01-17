%%%-------------------------------------------------------------------
%%% @author roeland
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Jan 2019 18:07
%%%-------------------------------------------------------------------
-module(location_tests).
-author("roeland").

-include_lib("eunit/include/eunit.hrl").

%% API
-export([]).

setup() ->
  meck:expect(survivor2, entry,
    fun(_) ->
      location_created
    end),
  LocationPid = location:create(resInstPid, locationTypPid),
  register(location, LocationPid).

cleanup(_) ->
  meck:unload().

location_test_() ->
  {setup,
    fun setup/0,
    fun cleanup/1,
    [{inorder,
      [ fun test_create/0,
        fun test_get_ResInst/0,
        fun test_get_Visitor_vacant/0,
        fun test_get_type/0,
        fun test_arrival_departure/0,
        fun test_dispose/0]}]}.

test_create() ->
  ?assert(erlang:is_process_alive(whereis(location))).

test_get_ResInst() ->
  {ok,ResInst} = location:get_ResInst(whereis(location)),
  ?assertEqual(resInstPid, ResInst).

test_get_Visitor_vacant() ->
  {ok, Vacant} = location:get_Visitor(whereis(location)),
  ?assertEqual(vacant, Vacant).

test_get_type() ->
  {ok, Type} = location:get_Type(whereis(location)),
  ?assertEqual(locationTypPid, Type).

test_arrival_departure() ->
  {arrived, Visitor} = location:arrival(whereis(location), visitorPid),
  {ok, Visitor} = location:get_Visitor(whereis(location)),
  ?assertEqual(visitorPid, Visitor),

  departed = location:departure(whereis(location)),
  {ok, Vacant} = location:get_Visitor(whereis(location)),
  ?assertEqual(vacant, Vacant).

test_dispose() ->
  remove = location:dispose(whereis(location)),
  timer:sleep(0),
  X = whereis(location),
  ?assertEqual(undefined, X),
  setup().

