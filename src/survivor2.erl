-module(survivor2).
-export([start/0, entry/1, init/0, printTable/0, printInst/1, printType/1]).

-spec start() -> 'true'.
start() ->
  (whereis(survivor) =:= undefined) orelse unregister(survivor),
  register(survivor, spawn(?MODULE, init, [])).

-spec entry(Data::_) -> 'true'.
entry(Data)->
  ets:insert(logboek, {{now(), self()}, Data}).

-spec init() -> 'ok'.
init() ->
  (ets:info(logboek) =:= undefined) orelse ets:delete(logboek),
  ets:new(logboek, [named_table, ordered_set, public]),
  loop().

-spec loop() -> 'ok'.
loop() ->
  receive
    stop -> ok
  end.

printTable() ->
  ets:match_object(logboek, {'$0', '$1'}).

printInst(Inst) ->
  ets:match_object(logboek, {'$0', {list_to_atom(lists:flatten(io_lib:format("~pInst_created", [Inst]))), '$1'}}).

printType(Type) ->
  ets:match_object(logboek, {'$0', list_to_atom(lists:flatten(io_lib:format("~pType_created", [Type])))}).
