-module(fluidumInst).

-export([create/2, init/2, get_resource_circuit/1]).

-spec create(Root_ConnectorPid::pid(),ResTyp_Pid::pid()) -> {'ok',pid()}.
create(Root_ConnectorPid, ResTyp_Pid) ->
	{ok, spawn(?MODULE, init, [Root_ConnectorPid, ResTyp_Pid])}.

-spec init(Root_ConnectorPid::pid(),ResTyp_Pid::pid()) -> no_return().
init(Root_ConnectorPid, ResTyp_Pid) ->
	{ok, State} = apply(resource_type, get_initial_state, [ResTyp_Pid, self(), [Root_ConnectorPid, plain_water]]),
	survivor2:entry({ fluidInst_created, State }),
	loop(Root_ConnectorPid, State, ResTyp_Pid).

-spec get_resource_circuit(ResInstPid::pid()) -> {'ok',_} | {'error','timed_out',pid(),_,reference()}.
get_resource_circuit(ResInstPid) ->
	msg:get(ResInstPid, get_resource_circuit). 

-spec loop(Root_ConnectorPid::pid(),State::_,ResTyp_Pid::pid()) -> no_return().
loop(Root_ConnectorPid, State, ResTyp_Pid) ->
	receive
		{get_locations, ReplyFn} ->
			{ok, L_List} = resource_type:get_locations_list(ResTyp_Pid, State), 
			ReplyFn(L_List),
			loop(Root_ConnectorPid, State, ResTyp_Pid);
		{get_type, ReplyFn} ->
			ReplyFn(ResTyp_Pid),
			loop(Root_ConnectorPid, State, ResTyp_Pid);
		{get_resource_circuit, ReplyFn} ->
			{ok, C} = fluidumTyp:get_resource_circuit(ResTyp_Pid, State),
			ReplyFn(C), 
			loop(Root_ConnectorPid, State, ResTyp_Pid);
		fluidum_arrives_at_locations ->
			{ok, C} = fluidumTyp:get_resource_circuit(ResTyp_Pid, State),
			arriveAtLocations(C, self()),
			loop(Root_ConnectorPid, State, ResTyp_Pid)
	end.

arriveAtLocations(C, Visitor) ->
	arriveAtLocations(maps:next(maps:iterator(C)), Visitor);

arriveAtLocations({C, _ , Iter }, Visitor) ->
	{ok, [Location |_]} = resource_instance:list_locations(C),
	location:arrival(Location, Visitor);
arriveAtLocations(_,_) -> ok.