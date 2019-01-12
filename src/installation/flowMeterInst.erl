-module(flowMeterInst).
-export([create/4, init/4, estimate_flow/1, measure_flow/1]).
% -export([commission/1, activate/1]).
% -export([deactivate/1, decommission/1]).

% FlowMeter is a pipe and possibly a more complex resource;  
% this resource instance is passed to the create function.
% RealWorldCmdFn is a function to read out the real-world flowMeter. 

-spec create(Host::_,FlowMeterTyp_Pid::pid(),ResInst_Pid::pid(),RealWorldCmdFn::_) -> {'ok',pid()}.
create(Host, FlowMeterTyp_Pid, ResInst_Pid, RealWorldCmdFn) -> 
	{ok, spawn(?MODULE, init, [Host, FlowMeterTyp_Pid, ResInst_Pid, RealWorldCmdFn])}.

-spec init(Host::_,FlowMeterTyp_Pid::pid(),ResInst_Pid::pid(),RealWorldCmdFn::_) -> no_return().
init(Host, FlowMeterTyp_Pid, ResInst_Pid, RealWorldCmdFn) ->
	{ok, State} = apply(resource_type, get_initial_state, [FlowMeterTyp_Pid, self(),     [ResInst_Pid, RealWorldCmdFn]]),
									%  get_initial_state  (ResTyp_Pid,       ThisResInst, TypeOptions)
	survivor2:entry({ flowMeterInst_created, State }),
	loop(Host, State, FlowMeterTyp_Pid, ResInst_Pid).

-spec estimate_flow(FlowMeterInst_Pid::pid()) -> {'ok',_} | {'error','timed_out',pid(),_,reference()}.
estimate_flow(FlowMeterInst_Pid) ->
	msg:get(FlowMeterInst_Pid, estimate_flow). 

-spec measure_flow(FlowMeterInst_Pid::pid()) -> {'ok',_} | {'error','timed_out',pid(),_,reference()}.
measure_flow(FlowMeterInst_Pid) ->
	msg:get(FlowMeterInst_Pid, measure_flow).  


-spec loop(Host::_,State::_,FlowMeterTyp_Pid::pid(),ResInst_Pid::pid()) -> no_return().
loop(Host, State, FlowMeterTyp_Pid, ResInst_Pid) ->
	receive
		{measure_flow, ReplyFn} ->
			{ok, Answer} = msg:get(FlowMeterTyp_Pid, measure_flow, State), 
			ReplyFn(Answer), 
			loop(Host, State, FlowMeterTyp_Pid, ResInst_Pid);
		{estimate_flow, ReplyFn} ->
			{ok, InfluenceFn} = msg:get(FlowMeterTyp_Pid, estimate_flow, State),
			ReplyFn(InfluenceFn),
			loop(Host, State, FlowMeterTyp_Pid, ResInst_Pid);
		{get_type, ReplyFn} -> 
			ReplyFn(FlowMeterTyp_Pid),
			loop(Host, State, FlowMeterTyp_Pid, ResInst_Pid);		
		OtherMessage -> 
			ResInst_Pid ! OtherMessage,
			loop(Host, State, FlowMeterTyp_Pid, ResInst_Pid)
	end.
