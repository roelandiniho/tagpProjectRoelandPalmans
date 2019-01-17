-module(location).
-export([create/2, get_ResInst/1, get_Visitor/1, get_Type/1, arrival/2, departure/1, dispose/1, set_ResInst/2]).
-export([init/2]).

-spec create(ResInst_Pid::pid(),LocationTyp_Pid::pid()|'emptySpace') -> pid().
create(ResInst_Pid, LocationTyp_Pid) ->
	spawn(?MODULE, init, [ResInst_Pid, LocationTyp_Pid]).

-spec init(ResInst_Pid::pid(),LocationTyp_Pid::pid()) -> 'stoppedVisitor_Pid'.
init(ResInst_Pid, LocationTyp_Pid) ->
	survivor2:entry(location_created),
	loop(ResInst_Pid, LocationTyp_Pid, vacant).

-spec get_ResInst(Location_Pid::pid()) -> {'ok',pid()} | {'error','timed_out',pid(),_,reference()}.
get_ResInst(Location_Pid) -> 
	msg:get(Location_Pid, get_ResInst).

-spec set_ResInst(Location_Pid::pid(), NewResInst::pid()) -> {'set_ResInst',_}.
set_ResInst(Location_Pid, NewResInst) ->
	Location_Pid ! {set_ResInst, NewResInst}.

-spec get_Visitor(Location_Pid::pid()) -> {'ok',pid()} | {'error','timed_out',pid(),_,reference()}.
get_Visitor(Location_Pid) ->
	msg:get(Location_Pid, get_Visitor).

-spec get_Type(Location_Pid::pid()) -> {'ok',pid()} | {'error','timed_out',pid(),_,reference()}.
get_Type(Location_Pid) ->
	msg:get(Location_Pid, get_Type).

-spec arrival(Location_Pid::pid(), Visitor_Pid::pid()) -> {'arrived',pid()}.
arrival(Location_Pid, Visitor_Pid) ->
	Location_Pid ! {arrived, Visitor_Pid}.

-spec departure(Location_Pid::pid()) -> 'departed'.
departure(Location_Pid) ->
	Location_Pid ! departed.

-spec dispose(Location_Pid::pid()) -> 'remove'.
dispose(Location_Pid) ->
	Location_Pid ! remove.

-spec loop(ResInst_Pid::pid(),LocationTyp_Pid::_,Visitor_Pid::pid()|'vacant') -> 'stoppedVisitor_Pid'.
loop(ResInst_Pid, LocationTyp_Pid, Visitor_Pid) -> 
	receive
		{get_ResInst, ReplyFn} -> 
			ReplyFn(ResInst_Pid),
			loop(ResInst_Pid, LocationTyp_Pid, Visitor_Pid);
		{get_Visitor, ReplyFn} -> 
			ReplyFn(Visitor_Pid),
			loop(ResInst_Pid, LocationTyp_Pid, Visitor_Pid);
		{get_Type, ReplyFn} -> 
			ReplyFn(LocationTyp_Pid),
			loop(ResInst_Pid, LocationTyp_Pid, Visitor_Pid);
		{set_ResInst, NewResInst} ->
			loop(NewResInst, LocationTyp_Pid, Visitor_Pid);
		{arrived, V_Pid} ->
			loop(ResInst_Pid, LocationTyp_Pid, V_Pid);
		departed -> 
			loop(ResInst_Pid, LocationTyp_Pid, vacant);
		remove -> 
			stoppedVisitor_Pid
	end. 