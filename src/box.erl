% simulator of link discovery service in ivanOS
%
% Cloudozer


-module(box).
-export([new/3,
		box/3,
		to_dot/2
		]).




new(Box,Ports_nbr,Links_pid) ->
	MACs = [ port:get_mac() || _ <- lists:seq(1,Ports_nbr) ],
	Bid = spawn(?MODULE,box,[Box,MACs, Links_pid]),
	register(Box,Bid),

	lists:foreach(  fun(M) -> spawn(port,new, [M,Box,Links_pid]) 
					end, MACs),
	Bid.
	



box(Box,Ports,Links_pid) -> 
	Net_data = neph:new(Box),
	box(Box,Ports,Links_pid,Net_data,[],undef).

box(Box,Ports,Links_pid,Net_data,Arch,Monitor) ->
	receive

	%% Requests from my ports

		{new_connection, My_port, Neighbor_port,Neighbor_box } -> 
			
			io:format("  ~p: new_connection - ~p~n",[Box,Neighbor_box]),
			Net_data1 = add_connection(Box,My_port,Neighbor_port,Neighbor_box,Net_data,Monitor),
			TS = os:timestamp(),
			Pkt = {<<"FFFFFF">>,My_port,<<"ND">>,{connected,TS,Box,Neighbor_port,Neighbor_box} },	
			broadcast(Ports,My_port,Links_pid,Pkt),
			box(Box,Ports,Links_pid,Net_data1,[TS|Arch],Monitor);
	
		{lost_connection, My_port} ->
			io:format("  ~p: lost connection in port ~s~n",[Box,port:pp(My_port)]),
			Net_data1 = del_connection(Box,My_port,Net_data,Monitor),
			TS = os:timestamp(),
			Pkt = {<<"FFFFFF">>,My_port,<<"ND">>,{disconnected,TS,Box} },	
			broadcast(Ports,My_port,Links_pid,Pkt),			
			box(Box,Ports,Links_pid,Net_data1,[TS|Arch],Monitor);
			
	%% Updates from the network

		{pkt,Port,{<<"FFFFFF">>,S_Port,<<"ND">>,{disconnected,TS,S_Box} }=Pkt } ->
			case lists:member(TS,Arch) of
				true -> 
					box(Box,Ports,Links_pid,Net_data,Arch,Monitor);  %% old packet - stop broadcasting message
				false ->
					broadcast(Ports,Port,Links_pid,Pkt),
					
					Net_data1 = del_connection(S_Box,S_Port,Net_data,Monitor),
					box(Box,Ports,Links_pid,Net_data1,[TS|Arch],Monitor)
			end;

		{pkt,Port,{<<"FFFFFF">>,Port1,<<"ND">>,{connected,TS,Box1,Port2,Box2} }=Pkt } ->
			case lists:member(TS,Arch) of
				true -> 
					box(Box,Ports,Links_pid,Net_data,Arch,Monitor);  %% old packet - stop broadcasting message
				false ->
					broadcast(Ports,Port,Links_pid,Pkt),
					
					Net_data1 = add_connection(Box1,Port1,Port2,Box2,Net_data,Monitor),
					box(Box,Ports,Links_pid,Net_data1,[TS|Arch],Monitor)
			end;	

	%% Requests from network Emulator (Shell)

		{get_info,Pid} ->
			Info = lists:foldl( fun(B,Acc)-> [{B,neph:neighbors(B,Net_data)}|Acc]
								end,[],neph:box_list(Net_data)),
			Pid ! {network_info,Info},
			box(Box,Ports,Links_pid,Net_data,Arch,Monitor);

		{get_net,Pid} ->
			Pid ! Net_data,
			box(Box,Ports,Links_pid,Net_data,Arch,Monitor);

		{draw_net,_} ->
			to_dot(Box,Net_data),
			box(Box,Ports,Links_pid,Net_data,Arch,Monitor);

		{ports_info,Pid} ->
			Pid ! {ports_info,Ports},
			box(Box,Ports,Links_pid,Net_data,Arch,Monitor);

	%% Requests from Web Monitor

		{get_entire_net, Pid} -> 
			%io:format("Got message from WS~n"),
			{Boxes,Wires} = neph:box_wire_list(Box,Net_data),
			%io:format("Boxes:~p~nWires:~p~n",[Boxes,Wires]),
			Pid ! {entire_net,Boxes,Wires},
			box(Box,Ports,Links_pid,Net_data,Arch,Pid)
	after
		2000 ->
			[Port|Rest] = Ports,
			case neph:neighbor(Box,Port,Net_data) of
				{Nei_port,Nei_box} ->
					TS = os:timestamp(),
					Pkt = {<<"FFFFFF">>,Port,<<"ND">>,{connected,TS,Box,Nei_port,Nei_box} },	
					broadcast(Ports,[],Links_pid,Pkt),
					box(Box,Rest++[Port],Links_pid,Net_data,[TS|Arch],Monitor);
				not_connected ->
					%TS = os:timestamp(),
					%Pkt = {<<"FFFFFF">>,Port,<<"ND">>,{disconnected,TS,Box} },	
					%broadcast(Ports,Port,Links_pid,Pkt)
					box(Box,Rest++[Port],Links_pid,Net_data,Arch,Monitor)	
			end
	end.




%% adds new connection to Net_data and sends update to Monitor
% returns new Net_data
add_connection(Box1,Port1,Port2,Box2,Net_data,undef) -> 
	neph:add_neighbor(Box1,Port1,Port2,Box2,Net_data);
add_connection(Box1,Port1,Port2,Box2,Net_data,Monitor) ->
	case {neph:has_box(Box1,Net_data), neph:has_box(Box2,Net_data)} of
		{false,false} -> Monitor ! {add_box, Box1}, Monitor ! {add_box, Box2}, Monitor ! {add_wire,Box1,Box2};
		{false,true} -> Monitor ! {add_box, Box1}, Monitor ! {add_wire,Box1,Box2};
		{true,false} -> Monitor ! {add_box, Box2}, Monitor ! {add_wire,Box1,Box2};
		{true,true} ->
			case neph:neighbor(Box1,Port1,Net_data) of
				{Port2,Box2} -> ok;
				_ -> Monitor ! {add_wire,Box1,Box2}
			end
	end,
	neph:add_neighbor(Box1,Port1,Port2,Box2,Net_data).




%% delets the connection in Net_data and sends update to Monitor
% returns new Net_data
del_connection(Box1,Port1,Net_data,Monitor) ->
	case neph:neighbor(Box1,Port1,Net_data) of
		{Port2,Box2} -> 
			case Monitor of
				undef -> ok;
				_ -> Monitor ! {del_wire,Box1,Box2}
			end,
			neph:del_wire(Box1,Port1,Port2,Box2,Net_data);
		not_connected -> 
			Net_data
	end.




%% sends {pkt,Port,Pkt} to to Links_pid for each port excepts a given Port_not_send
broadcast(Ports_to_send,Port_not_send,Links_pid,Pkt) ->						
	lists:foreach(  fun(P) ->
						case P of
							Port_not_send -> ok;
							_ -> Links_pid ! {pkt,P,Pkt}
						end
					end,Ports_to_send).




to_dot(Box,Net_data) ->
	if
		is_atom(Box) -> File = atom_to_list(Box)++".dot";
		is_list(Box) -> File = Box++".dot";
		true -> File = "network.dot"
	end,

	{ok,Dev} = file:open(File,write),

	io:format(Dev,"graph ~p {~n\tnode [shape=box,style=filled,color=grey];~n",[Box]),
	io:format(Dev,"\t ~p [color=yellow];~n",[Box]),

	{Edges,_} = lists:foldl(fun(B,{Acc,Processed_boxes}) -> 

		Neibs = [ Nb || Nb <- neph:neighbor_boxes(B,Net_data), not lists:member(Nb,Processed_boxes) ],
		{[ {Nb,B} || Nb <- Neibs]++Acc, [B|Processed_boxes]}

							end,{[],[]},neph:box_list(Net_data)),

	%io:format("Edges:~p~n",[Edges]),
	lists:foreach(  fun({B1,B2}) ->

		io:format(Dev,"\t~p -- ~p;~n",[B1,B2])
					end, Edges),

	io:format(Dev,"}~n",[]),
	file:close(Dev).


