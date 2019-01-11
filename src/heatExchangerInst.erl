-module(heatExchangerInst).
-export([create/4, init/4, temp_influence/1]).

% HeatExchanger is a pipe and more; this pipe instance is passed to the create function.
% HeatExchangers have a HE_link to, typically, another HeatExchanger. The link provides 
% a function that models the mutual effect on the temperature of the flows on either side. 

-spec create(Host::_,HeatExchangerTyp_Pid::pid(),PipeInst_Pid::pid(),HE_link_spec::_) -> {'ok',pid()}.
create(Host, HeatExchangerTyp_Pid, PipeInst_Pid, HE_link_spec) -> {ok, spawn(?MODULE, init, [Host, HeatExchangerTyp_Pid, PipeInst_Pid, HE_link_spec])}.

-spec init(Host::_,HeatExchangerTyp_Pid::pid(),PipeInst_Pid::pid(),HE_link_spec::_) -> no_return().
init(Host, HeatExchangerTyp_Pid, PipeInst_Pid, HE_link_spec) ->
	{ok, State} = apply(resource_type, get_initial_state, [HeatExchangerTyp_Pid, self(), PipeInst_Pid]),
									%  get_initial_state  (ResTyp_Pid,  ResInst_Pid, TypeOptions) 
	survivor2:entry({ pumpInst_created, State }),
	loop(Host, State, HeatExchangerTyp_Pid, PipeInst_Pid, HE_link_spec).



-spec temp_influence(HeatExchangerInst_Pid::pid()) -> {'ok',_} | {'error','timed_out',pid(),_,reference()}.
temp_influence(HeatExchangerInst_Pid) ->
	msg:get(HeatExchangerInst_Pid, get_temp_influence).


-spec loop(Host::_,State::_,HeatExchangerTyp_Pid::pid(),PipeInst_Pid::pid(),HE_link_spec::_) -> no_return().
loop(Host, State, HeatExchangerTyp_Pid, PipeInst_Pid, HE_link_spec) ->
	receive
		{get_type, ReplyFn} -> 
			ReplyFn(HeatExchangerTyp_Pid),
			loop(Host, State, HeatExchangerTyp_Pid, PipeInst_Pid, HE_link_spec);
		{get_temp_influence, ReplyFn} ->
			ReplyFn(heatExchangeLink:get_temp_influence(HE_link_spec)),
			loop(Host, State, HeatExchangerTyp_Pid, PipeInst_Pid, HE_link_spec);
		OtherMessage -> 
			PipeInst_Pid ! OtherMessage,
			loop(Host, State, HeatExchangerTyp_Pid, PipeInst_Pid, HE_link_spec)
	end.
