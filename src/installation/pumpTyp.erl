-module(pumpTyp).
-export([create/0, init/0]).
% -export([dispose/2, enable/2, new_version/2]).
% -export([get_initial_state/3, get_connections_list/2]). % use resource_type
% -export([update/3, execute/7, refresh/4, cancel/4, update/7, available_ops/2]). 

-spec create() -> {'ok',pid()}.
create() ->
	{ok, spawn(?MODULE, init, [])}.

-spec init() -> no_return().
init() ->
	survivor2:entry(pumpTyp_created),
	loop().

-spec loop() -> no_return().
loop() ->
	receive
%    get_initial_state, self(),     ]
		{initial_state, [ResInst_Pid, [PipeInst_Pid, RealWorldCmdFn]], ReplyFn} ->
			ReplyFn(#{resInst => ResInst_Pid, pipeInst => PipeInst_Pid, 
					  rw_cmd => RealWorldCmdFn, on_or_off => off}), 
			loop();
		{switchOff, State, ReplyFn} -> 
			#{rw_cmd := ExecFn} = State, ExecFn(off), 
			ReplyFn(State#{on_or_off := off}),
			loop(); 
		{switchOn, State, ReplyFn} ->
			#{rw_cmd := ExecFn} = State, ExecFn(on), 
			ReplyFn(State#{on_or_off := on}),
			loop(); 
		{isOn, State, ReplyFn} -> 
			#{on_or_off := OnOrOff} = State, 
			ReplyFn(OnOrOff),
			loop();
		{flow_influence, State, ReplyFn} -> 
			#{on_or_off := OnOrOff} = State,
			FlowInfluenceFn = fun(Flow) -> flow(Flow, OnOrOff) end, % placeholder only.
			ReplyFn(FlowInfluenceFn), 
			loop()
	end. 

-spec flow(_,'off' | 'on') -> number().
flow(Flow, on)  -> (250 - 5 * Flow - 2 * Flow * Flow);
flow(_Flow, off) -> 0. 