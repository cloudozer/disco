% simulator of link discovery service in ivanOS
%
%


-module(box).
-export([new/3,
		box/3,
		get_mac/0,
		to_dot/2,
		port/1
		]).



new(Box_id,Ports_nbr,Links_pid) ->
	Ports = [ get_mac() || _ <- lists:seq(1,Ports_nbr) ],
	Box_pid = spawn(?MODULE,box,[Box_id,Ports, Links_pid]),

	lists:foreach(fun(P)-> Links_pid ! {new_port,P,Box_pid} end, Ports),
	Nd = spawn(nd,nd,[Box_id,Box_pid,neph:new(Box_id),Ports,Links_pid]),
	register(Box_id,Nd),

	Box_pid.



box(Box_id,Ports,Links_pid) ->
	receive
		%{msg_out,Port}

		{pkt,Port,nd,Pkt} ->
			Box_id ! {pkt,Port,Pkt}, %% send to nd process
			box(Box_id,Ports,Links_pid);

		{pkt_out,Port,Pkt} ->
			Links_pid ! {pkt_in,Port,Pkt},
			box(Box_id,Ports,Links_pid);		

		{network_info,Pid} ->
			Box_id ! {network_info,Pid},
			box(Box_id,Ports,Links_pid);

		{ports_info,Pid} ->
			Pid ! {ports_info,Ports},
			box(Box_id,Ports,Links_pid);

		_ -> box(Box_id,Ports,Links_pid)
	end.






port(<<P1,P2,P3,P4,P5,P6>>) -> 
	byte_to_hex(P1)++":"++byte_to_hex(P2)++":"++
	byte_to_hex(P3)++":"++byte_to_hex(P4)++":"++
	byte_to_hex(P5)++":"++byte_to_hex(P6). 


byte_to_hex(B) ->
	S = integer_to_list(B,16),
	case length(S) of
		2 -> S;
		1 -> [$0|S]
	end.



get_mac() -> crypto:strong_rand_bytes(6).
	


to_dot(File,Network) ->
	{ok,Dev} = file:open(File,write),

	io:format(Dev,"graph Network {~n\tnode [shape=box,style=filled,color=grey];~n",[]),

	lists:foreach(  fun({W,B1,B2}) ->
		io:format(Dev,"\t~p -- ~p [label=~p];~n",[B1,B2,W])
					end, Network),

	io:format(Dev,"}~n",[]),
	file:close(Dev).


