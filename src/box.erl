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

-define(PING_INTERVAL,20000).
-define(PING_TIMEOUT,1000).



new(Box_id,Ports_nbr,Links_pid) ->
	Ports = [ get_mac() || _ <- lists:seq(1,Ports_nbr) ],
	Box_pid = spawn(?MODULE,box,[Box_id,Ports, Links_pid]),

	lists:foreach(fun(P)-> Links_pid ! {new_port,P,Box_pid} end, Ports),
	Box_pid.



box(Box_id,Ports,Links_pid) -> box(Box_id,Ports,Links_pid,neph:new(Box_id),undef,undef).

box(Box_id,Ports,Links_pid,Net_data,undef,undef) ->
	receive
		%{msg_out,Port}

		{pkt,Port,{<<"FFFFFF">>,Source_port,{ping,Source_box} }} ->
			% update your Network info and ping back
			io:format("Port ~s of ~p got PING from ~p~n",[port(Port),Box_id,Source_box]),
			Links_pid ! {pkt, Port, {Source_port,Port,{ping_resp,Box_id}} },
			case neph:has_box(Source_box,Net_data) of
				false ->
					Net_data1 = neph:add_neighbor(Box_id,Port,Source_port,Source_box,Net_data),
					box(Box_id,Ports,Links_pid,Net_data1,undef,undef);
				true ->
					box(Box_id,Ports,Links_pid,Net_data,undef,undef)
			end;

		{get_info,Pid} ->
			Pid ! {network_info,neph:boxes(Net_data)},
			box(Box_id,Ports,Links_pid,Net_data,undef,undef);

		{ports_info,Pid} ->
			Pid ! {ports_info,Ports},
			box(Box_id,Ports,Links_pid,Net_data,undef,undef);

		_ -> box(Box_id,Ports,Links_pid,Net_data,undef,undef)

	after
		?PING_INTERVAL div length(Ports) ->
			[P|Tail] = Ports,
			Links_pid ! {pkt, P, {<<"FFFFFF">>,P,{ping,Box_id}} },
			box(Box_id,Tail++[P],Links_pid,Net_data,pinging,os:timestamp())

	end;
box(Box_id,Ports,Links_pid,Net_data,pinging,T1) ->
	receive
		%{msg_out,Port}

		{pkt,Port,{<<"FFFFFF">>,Source_port,{ping,Source_box} }} ->
			% update your Network info and ping back
			io:format("Port ~s of ~p got PING from ~p~n",[port(Port),Box_id,Source_box]),
			Links_pid ! {pkt, Port, {Source_port,Port,{ping_resp,Box_id}} },
			case neph:has_box(Source_box,Net_data) of
				false ->
					Net_data1 = neph:add_neighbor(Box_id,Port,Source_port,Source_box,Net_data),
					box(Box_id,Ports,Links_pid,Net_data1,pinging,T1);
				true ->
					box(Box_id,Ports,Links_pid,Net_data,pinging,T1)
			end;

		{pkt,Port,{Port,Source_port,{ping_resp,Source_box} }} ->
			T2 = os:timestamp(),
			io:format("Port ~s of ~p got ping back from ~p in ~pus~n",[port(Port),Box_id,Source_box,timer:now_diff(T2,T1)]),
			case neph:has_box(Source_box,Net_data) of
				false ->
					Net_data1 = neph:add_neighbor(Box_id,Port,Source_port,Source_box,Net_data),
					box(Box_id,Ports,Links_pid,Net_data1,undef,undef);
				true ->
					box(Box_id,Ports,Links_pid,Net_data,undef,undef)
			end;		

		{get_info,Pid} ->
			Pid ! {network_info,neph:boxes(Net_data)},
			box(Box_id,Ports,Links_pid,Net_data,pinging,T1);

		{ports_info,Pid} ->
			Pid ! {ports_info,Ports},
			box(Box_id,Ports,Links_pid,Net_data,pinging,T1);

		_ -> box(Box_id,Ports,Links_pid,Net_data,pinging,T1)

	after
		?PING_TIMEOUT ->
			% check if connection lost
			P = lists:last(Ports),
			case neph:neighbor(Box_id,P,Net_data) of
				{Neighbor_port,Neighbor_box} ->
					io:format("Link between ~p and ~p over ~s--~s lost~n",
						[Box_id,Neighbor_box,port(P),port(Neighbor_port)]),
					% Update Net_data
					Net_data1 = neph:del_wire(Box_id,P,Neighbor_port,Neighbor_box,Net_data),
					box(Box_id,Ports,Links_pid,Net_data1,undef,undef);
				not_connected ->
					box(Box_id,Ports,Links_pid,Net_data,undef,undef)
			end

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


