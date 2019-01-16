-module(pipeInst).
-export([create/2, init/2, get_flow_influence/1, set_ResInst_connector/2, set_ResInst_location/2]).

-spec create(Host::_,ResTyp_Pid::pid()) -> {'ok',pid()}.
create(Host, ResTyp_Pid) ->
	{ok, spawn(?MODULE, init, [Host, ResTyp_Pid])}.

-spec init(Host::_,ResTyp_Pid::pid()) -> no_return().
init(Host, ResTyp_Pid) ->
%	{ok, State} = apply(resource_type, get_initial_state, [ResTyp_Pid, self(), []]),	
	{ok, State} = resource_type:get_initial_state(ResTyp_Pid, self(), []),
	survivor2:entry({ pipeInst_created, State }),
	loop(Host, State, ResTyp_Pid).

-spec get_flow_influence(PipeInst_Pid::pid()) -> {'ok',_} | {'error','timed_out',pid(),_,reference()}.
get_flow_influence(PipeInst_Pid) ->
	msg:get(PipeInst_Pid, get_flow_influence).

set_ResInst_connector(PipeInst_Pid, NewResInst) ->
	PipeInst_Pid ! {setConnectorResInst, NewResInst}.

set_ResInst_location(PipeInst_Pid, NewResInst) ->
	PipeInst_Pid ! {setLocationResInst, NewResInst}.

-spec loop(Host::_,State::_,ResTyp_Pid::pid()) -> no_return().
loop(Host, State, ResTyp_Pid) ->
	receive
		{get_connectors, ReplyFn} ->
			{ok,C_List} = resource_type:get_connections_list(ResTyp_Pid, State), 
			ReplyFn(C_List),
			loop(Host, State, ResTyp_Pid);
		{get_locations, ReplyFn} ->
			{ok, List} = resource_type:get_locations_list(ResTyp_Pid, State),
			ReplyFn(List),
			loop(Host, State, ResTyp_Pid);
		{get_type, ReplyFn} ->
			ReplyFn(ResTyp_Pid),
			loop(Host, State, ResTyp_Pid);
		{set_ResInst_connector, NewResInst_Pid} ->
			{ok,C_List} = resource_type:get_connections_list(ResTyp_Pid, State),
			[In, Out] = C_List,
			connector:set_ResInst(In, NewResInst_Pid),
			connector:set_ResInst(Out, NewResInst_Pid),
			loop(Host, State, ResTyp_Pid);
		{set_ResInst_location, NewResInst_Pid} ->
			{ok,L_List} = resource_type:get_locations_list(ResTyp_Pid, State),
			location:set_ResInst(L_List,  NewResInst_Pid),
			loop(Host, State, ResTyp_Pid);
		{get_ops, ReplyFn} ->
			ReplyFn([]),
			loop(Host, State, ResTyp_Pid);
		{get_state, ReplyFn} ->
			ReplyFn(State),
			loop(Host, State, ResTyp_Pid);
		{get_flow_influence, ReplyFn} ->
			{ok, InfluenceFn} = msg:get(ResTyp_Pid, flow_influence, State),
			ReplyFn(InfluenceFn),
			loop(Host, State, ResTyp_Pid);
		{set_host, NewHost} ->
			loop(NewHost, State, ResTyp_Pid);
		{get_host, ReplyFn} ->
			ReplyFn(Host),
			loop(Host, State, ResTyp_Pid)
	end.