%%%-------------------------------------------------------------------
%%% @author roeland
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Jan 2019 16:18
%%%-------------------------------------------------------------------
-module(connector_tests).
-author("roeland").

-include_lib("eunit/include/eunit.hrl").

%% API
-export([]).

setup() ->
  meck:new(survivor2),
  meck:expect(survivor2, entry,
    fun(_) ->
      connector_created
    end),
  ConnectorPidIn = connector:create(resInstPid, connecterTyp_PIDIn),
  ConnectorPidOut = connector:create(resInstPid, connecterTyp_PIDOut),
  register(connectorPidIn, ConnectorPidIn),
  register(connectorPidOut, ConnectorPidOut).

cleanup(_) ->
  unregister(connectorPidOut),
  meck:unload().

connector_test_() ->
  {setup,
    fun setup/0,
    fun cleanup/1,
    [{inorder,
      [fun test_create/0,
        fun test_connect/0,
        fun test_get_connected/0,
        fun test_disconnect/0,
        fun test_get_ResInst/0,
        fun test_get_type/0,
        fun test_discard/0]}]}.

test_create() ->
  ?assert(erlang:is_process_alive(whereis(connectorPidIn))),
  ?assert(erlang:is_process_alive(whereis(connectorPidOut))).

test_connect() ->
  {connect, Connect} = connector:connect(whereis(connectorPidIn), whereis(connectorPidOut)),
  ?assertEqual(whereis(connectorPidOut), Connect).

test_get_connected() ->
  List = connector:get_connected(whereis(connectorPidIn)),
  ?assertEqual({ok, whereis(connectorPidOut)}, List).

test_disconnect() ->
  Disconnect = connector:disconnect(whereis(connectorPidIn)),
  ?assertEqual(disconnect, Disconnect).

test_get_ResInst() ->
  {ok, RestInst} = connector:get_ResInst(whereis(connectorPidIn)),
  ?assertEqual(resInstPid, RestInst).

test_get_type() ->
  {ok, ConnectorTypPid} = connector:get_type(whereis(connectorPidIn)),
  ?assertEqual(connecterTyp_PIDIn, ConnectorTypPid).

test_discard() ->
  meck:expect(survivor2, entry,
    fun(_) ->
      connector_discarded
    end),
  Discard = connector:discard(whereis(connectorPidIn)),
  ?assertEqual(discard, Discard).

