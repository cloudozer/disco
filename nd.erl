% simulator of link discovery service in ivanOS
%
%


-module(nd).
-export([nd/5
		]).

-define(PING_INTERVAL,1000).



nd(Box_id,Box_pid,{Network,_}=Net_data,Ports,Links_pid) ->
	receive
		{network_info,Pid} ->
			Pid ! {network_info,dict:to_list(Network)},
			nd(Box_id,Box_pid,Net_data,Ports,Links_pid);

		{pkt, Port, {<<"FFFFFF">>,Source_port,{ping,Source_box} }} ->
			% update your Network info and ping back
			Box_pid ! {pkt_out, Port, {Source_port,Port,{ping_resp,Box_id}} },
			case neph:is_known(Source_box,Net_data) of
				false ->
					Net_data1 = neph:add_neighbor(Box_id,Port,Source_port,Source_box,Net_data),
					nd(Box_id,Box_pid,Net_data1,Ports,Links_pid);
				true ->
					nd(Box_id,Box_pid,Net_data,Ports,Links_pid)
			end;
		
		_ ->
			nd(Box_id,Box_pid,Net_data,Ports,Links_pid)

	after
		?PING_INTERVAL div length(Ports) ->
			[P|Tail] = Ports,
			Box_pid ! {pkt_out, P, {<<"FFFFFF">>,P,{ping,Box_id}} },
			nd(Box_id,Box_pid,Net_data,Tail++[P],Links_pid)

	end.



