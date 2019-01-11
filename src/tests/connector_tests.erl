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

create_test() ->
  ConnecterPid = connector:create(resInstPid, connecterTyp_PID),
  ?assert(erlang:is_process_alive(ConnecterPid)).

connect_test() ->
  meck:new(survivor2),
  meck:expect(survivor2, entry,
    fun(_) ->
      connection_made
    end),

  ConnecterPid = connector:create(resInstPid1, connecterTyp_PID1),
  ConnecterPid2 = connector:create(resInstPid2, connecterTyp_PID2),
  {connect, Connect} = connector:connect(ConnecterPid, ConnecterPid2),
  ?assertEqual(ConnecterPid2, Connect).

disconnect_test() ->
  ConnecterPid = connector:create(resInstPid1, connecterTyp_PID1),

  Disconnect = connector:disconnect(ConnecterPid),
  ?assertEqual(disconnect, Disconnect).

get_connected_test() ->
  ConnecterPid = connector:create(resInstPid1, connecterTyp_PID1),
  ConnecterPid2 = connector:create(resInstPid2, connecterTyp_PID2),
  connector:connect(ConnecterPid, ConnecterPid2),

  List = connector:get_connected(ConnecterPid),
  ?assertEqual({ok, ConnecterPid2}, List).

get_resInst_test() ->
  ConnecterPid = connector:create(resInstPid1, connecterTyp_PID1),
  {ok, RestInst} = connector:get_ResInst(ConnecterPid),
  ?assertEqual(resInstPid1, RestInst).

get_type_test() ->
  ConnecterPid = connector:create(resInstPid1, connecterTyp_PID1),
  {ok, ConnecterTypPid} = connector:get_type(ConnecterPid),
  ?assertEqual(connecterTyp_PID1, ConnecterTypPid).

discard_test() ->
  meck:new(survivor2),
  meck:expect(survivor2, entry,
    fun(_) ->
      connector_discarded
    end),

  ConnecterPid = connector:create(resInstPid1, connecterTyp_PID1),
  Discard = connector:discard(ConnecterPid),
  ?assertEqual(discard, Discard).

