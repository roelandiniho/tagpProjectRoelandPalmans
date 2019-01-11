%%%-------------------------------------------------------------------
%%% @author roeland
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Jan 2019 18:07
%%%-------------------------------------------------------------------
-module(locations_tests).
-author("roeland").

-include_lib("eunit/include/eunit.hrl").

%% API
-export([]).

create_test() ->
  {ok, FlowMeterPid} = flowMeterTyp:create(),
  ?assertNotEqual([], FlowMeterPid).