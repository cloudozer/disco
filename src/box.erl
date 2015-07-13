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

-define(PING_INTERVAL,30000).
-define(PING_TIMEOUT,500).



new(Box_id,Ports_nbr,Links_pid) ->
	Ports = [ get_mac() || _ <- lists:seq(1,Ports_nbr) ],
	Box_pid = spawn(?MODULE,box,[Box_id,Ports, Links_pid]),

	lists:foreach(fun(P)-> Links_pid ! {new_port,P,Box_pid} end, Ports),
	Box_pid.



box(Box_id,Ports,Links_pid) -> box(Box_id,Ports,Links_pid,neph:new(Box_id),[]).

box(Box_id,Ports,Links_pid,Net_data,Arch) ->
	receive
		
		{pkt,Port,{<<"FFFFFF">>,Source_port,{ping,Source_box} }} ->
			% update your Network info and ping back
			%io:format("Port ~s of ~p got PING from ~p~n",[port(Port),Box_id,Source_box]),
			Links_pid ! {pkt, Port, {Source_port,Port,{ping_resp,Box_id}} },
			case neph:has_box(Source_box,Net_data) of
				false ->
					Net_data1 = neph:add_neighbor(Box_id,Port,Source_port,Source_box,Net_data),
					% TODO - distribute info about new connection
					% send to the new box all info about existing network
					Arch1 = broadcast_net_info(Box_id,Port,Links_pid,Net_data1,Arch),

					box(Box_id,Ports,Links_pid,Net_data1,Arch1);
				true ->
					box(Box_id,Ports,Links_pid,Net_data,Arch)
			end;

		{pkt,Port,{<<"FFFFFF">>,_Source_port, {del_wire,Box1,P1,P2,Box2,TS}=Pkt} } ->
			% check if this message has been already received
			case lists:member(TS,Arch) of
				true -> box(Box_id,Ports,Links_pid,Net_data,Arch);
				false-> 
					lists:foreach(  fun(P) -> Links_pid ! {pkt,P,{<<"FFFFFF">>,P,Pkt }}
									end,lists:filter(fun(P)-> P=/=Port end,Ports)),
					Net_data1 = neph:del_wire(Box1,P1,P2,Box2,Net_data),
					box(Box_id,Ports,Links_pid,Net_data1,[TS|Arch])
			end;

		{pkt,Port,{<<"FFFFFF">>,_Source_port, {add_wire,Box1,P1,P2,Box2,TS}=Pkt} } ->
			% check if this message has been already received
			case lists:member(TS,Arch) of
				true -> box(Box_id,Ports,Links_pid,Net_data,Arch);
				false-> 
					lists:foreach(  fun(P) -> Links_pid ! {pkt,P,{<<"FFFFFF">>,P,Pkt }}
									end,lists:filter(fun(P)-> P=/=Port end,Ports)),
					Net_data1 = neph:add_neighbor(Box1,P1,P2,Box2,Net_data),
					box(Box_id,Ports,Links_pid,Net_data1,[TS|Arch])
			end;


		{get_info,Pid} ->
			Pid ! {network_info,neph:boxes(Net_data)},
			box(Box_id,Ports,Links_pid,Net_data,Arch);

		{draw_net,_} ->
			to_dot(Box_id,Net_data),
			box(Box_id,Ports,Links_pid,Net_data,Arch);

		{ports_info,Pid} ->
			Pid ! {ports_info,Ports},
			box(Box_id,Ports,Links_pid,Net_data,Arch);

		_ -> box(Box_id,Ports,Links_pid,Net_data,Arch)

	after
		?PING_INTERVAL div length(Ports) ->
			[P|Tail] = Ports,
			Links_pid ! {pkt, P, {<<"FFFFFF">>,P,{ping,Box_id}} },
			box(Box_id,Tail++[P],Links_pid,Net_data,Arch,pinging,os:timestamp())

	end.

%% a state when box sent a ping to its port and is waiting for the respond
box(Box_id,Ports,Links_pid,Net_data,Arch,pinging,T1) ->  
	receive
		
		{pkt,Port,{<<"FFFFFF">>,Source_port,{ping,Source_box} }} ->
			% update your Network info and ping back
			io:format("Port ~s of ~p got PING from ~p~n",[port(Port),Box_id,Source_box]),
			Links_pid ! {pkt, Port, {Source_port,Port,{ping_resp,Box_id}} },
			case neph:has_box(Source_box,Net_data) of
				false ->
					Net_data1 = neph:add_neighbor(Box_id,Port,Source_port,Source_box,Net_data),
					Arch1 = broadcast_net_info(Box_id,Port,Links_pid,Net_data1,Arch),
					box(Box_id,Ports,Links_pid,Net_data1,Arch1,pinging,T1);
				true ->
					box(Box_id,Ports,Links_pid,Net_data,Arch,pinging,T1)
			end;

		{pkt,Port,{Port,Source_port,{ping_resp,Source_box} }} ->
			T2 = os:timestamp(),
			io:format("Port ~s of ~p got ping back from ~p in ~pus~n",[port(Port),Box_id,Source_box,timer:now_diff(T2,T1)]),
			case neph:has_box(Source_box,Net_data) of
				false ->
					Net_data1 = neph:add_neighbor(Box_id,Port,Source_port,Source_box,Net_data),
					Arch1 = broadcast_net_info(Box_id,Port,Links_pid,Net_data1,Arch),
					box(Box_id,Ports,Links_pid,Net_data1,Arch1);
				true ->
					box(Box_id,Ports,Links_pid,Net_data,Arch)
			end;		

		{pkt,Port,{<<"FFFFFF">>,_Source_port, {del_wire,Box1,P1,P2,Box2,TS}=Pkt} } ->
			% check if this message has been already received
			case lists:member(TS,Arch) of
				true -> box(Box_id,Ports,Links_pid,Net_data,Arch,pinging,T1);
				false-> 
					lists:foreach(  fun(P) -> Links_pid ! {pkt,P,{<<"FFFFFF">>,P,Pkt }}
									end,lists:filter(fun(P)-> P=/=Port end,Ports)),
					Net_data1 = neph:del_wire(Box1,P1,P2,Box2,Net_data),
					box(Box_id,Ports,Links_pid,Net_data1,[TS|Arch],pinging,T1)
			end;

		{pkt,Port,{<<"FFFFFF">>,_Source_port, {add_wire,Box1,P1,P2,Box2,TS}=Pkt} } ->
			% check if this message has been already received
			case lists:member(TS,Arch) of
				true -> box(Box_id,Ports,Links_pid,Net_data,Arch,pinging,T1);
				false-> 
					lists:foreach(  fun(P) -> Links_pid ! {pkt,P,{<<"FFFFFF">>,P,Pkt }}
									end,lists:filter(fun(P)-> P=/=Port end,Ports)),
					Net_data1 = neph:add_neighbor(Box1,P1,P2,Box2,Net_data),
					box(Box_id,Ports,Links_pid,Net_data1,[TS|Arch],pinging,T1)
			end;


		{get_info,Pid} ->
			Pid ! {network_info,neph:boxes(Net_data)},
			box(Box_id,Ports,Links_pid,Net_data,Arch,pinging,T1);

		{ports_info,Pid} ->
			Pid ! {ports_info,Ports},
			box(Box_id,Ports,Links_pid,Net_data,Arch,pinging,T1);

		{draw_net,_} ->
			to_dot(Box_id,Net_data),
			box(Box_id,Ports,Links_pid,Net_data,Arch,pinging,T1);

		_ -> box(Box_id,Ports,Links_pid,Net_data,Arch,pinging,T1)

	after
		?PING_TIMEOUT ->
			% check if connection lost
			P = lists:last(Ports),
			case neph:neighbor(Box_id,P,Net_data) of
				{Lost_port,Lost_box} ->
					io:format("Link between ~p and ~p over ~s--~s lost~n",[Box_id,Lost_box,port(P),port(Lost_port)] ),
					% Update Net_data
					Net_data1 = neph:del_wire(Box_id,P,Lost_port,Lost_box,Net_data),
					% send an info_update to all its neighbors about network change
					TS=os:timestamp(),
					Pkt = {del_wire,Box_id,P,Lost_port,Lost_box,TS},

					lists:foreach(  fun(Port) -> Links_pid ! {pkt,Port,{<<"FFFFFF">>,Port,Pkt }}
									end,Ports),

					box(Box_id,Ports,Links_pid,Net_data1,[TS|Arch]);
				not_connected ->
					box(Box_id,Ports,Links_pid,Net_data,Arch)
			end

	end.


% sends out network info in a form of packets to a given port
% returns a new Archive of sent broadcasts
broadcast_net_info(Box_id,Port,Links_pid,Net_data,Arch) ->
	bni([Box_id],Port,Links_pid,Net_data,Arch,[]).

bni([Lead|Leads],Port,Links_pid,Net_data,Arch,Proc_boxes) ->
	Neibs = [ {P1,P2,B} || {P1,P2,B} <- neph:neighbors(Lead,Net_data), not lists:member(B,Proc_boxes)],
	Arch1 = lists:foldl(fun({P1,P2,B},Acc) ->
			TS = os:timestamp(),
			Links_pid ! {pkt,Port,{<<"FFFFFF">>,Port, {add_wire,Lead,P1,P2,B,TS}} },
			[TS|Acc] 
						end, Arch, Neibs),
	Boxes = lists:usort([ B || {_,_,B} <- Neibs]),
	bni(Boxes++Leads,Port,Links_pid,Net_data,Arch1,[Lead|Proc_boxes]);
bni([],_Port,_Links_pid,_Net_data,Arch,_Proc_boxes) -> Arch.


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
	


to_dot(Box_id,Net_data) ->
	if
		is_atom(Box_id) -> File = atom_to_list(Box_id)++".dot";
		is_list(Box_id) -> File = Box_id++".dot";
		true -> File = "network.dot"
	end,

	{ok,Dev} = file:open(File,write),

	io:format(Dev,"graph ~p {~n\tnode [shape=box,style=filled,color=grey];~n",[Box_id]),
	io:format(Dev,"\t ~p [color=yellow];~n",[Box_id]),

	{Edges,_} = lists:foldl(fun(Box,{Acc,Processed_boxes}) -> 

		Neibs = [ B || B <- neph:neighbor_boxes(Box,Net_data), not lists:member(B,Processed_boxes) ],
		{[ {B,Box} || B <- Neibs]++Acc, [Box|Processed_boxes]}

							end,{[],[]},neph:boxes(Net_data)),

	io:format("Edges:~p~n",[Edges]),
	lists:foreach(  fun({B1,B2}) ->

		io:format(Dev,"\t~p -- ~p;~n",[B1,B2])
					end, Edges),

	io:format(Dev,"}~n",[]),
	file:close(Dev).


