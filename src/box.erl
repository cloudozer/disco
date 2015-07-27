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
	box(Box,Ports,Links_pid,neph:new(Box),[],undef).

box(Box,Ports,Links_pid,Net_data,Arch,Monitor) ->
	receive

	%% Requests from my ports (BROADCASTs)

		{new_connection, My_port, Neighbor_port,Neighbor_box } -> 
			io:format("  ~p: new_connection - ~p~n",[Box,Neighbor_box]),
			%% check out if the connection is already known
			case neph:neighbor(Box,My_port,Net_data) of
				{Neighbor_port,Neighbor_box} ->  			%% do nothing
					box(Box,Ports,Links_pid,Net_data,Arch,Monitor); 

				not_connected ->
					%% update database and broadcast to neighbors
					Net_data1 = add_connection(Box,My_port,Neighbor_port,Neighbor_box,Net_data,Monitor),
					
					%% broadcast info about new wire
					TS = os:timestamp(),
					Pkt = {<<"FFFFFF">>,My_port,<<"ND">>,{add_wire,Box,My_port,Neighbor_port,Neighbor_box,TS} },	
					broadcast(Ports,My_port,Links_pid,Pkt),
					
					%% checkout if the Neighbor box is a member of the network
					%case neph:has_box(Neighbor_box,Net_data) of
					%	true -> box(Box,Ports,Links_pid,Net_data1,[TS|Arch]);						
					%		
					%	false->	
					%		%% send net_info to a new box
					%		Arch1 = send_net_info(Box,My_port,Links_pid,Net_data,[TS|Arch]),
					%		box(Box,Ports,Links_pid,Net_data1,Arch1)
					%end
					
					%% send entire net_info in any case
					Arch1 = send_net_info(Box,My_port,Links_pid,Net_data,[TS|Arch]), 
					box(Box,Ports,Links_pid,Net_data1,Arch1,Monitor)
			end;

		{lost_connection, _My_port} ->
			box(Box,Ports,Links_pid,Net_data,Arch,Monitor);


		{pkt,Port,{<<"FFFFFF">>,_,<<"ND">>,{add_wire,Box1,Port1,Port2,Box2,TS} }=Pkt } ->
			io:format("~p: got broadcast - ~p~n",[Box,Pkt]),
			case lists:member(TS,Arch) of
				true -> 
					box(Box,Ports,Links_pid,Net_data,Arch,Monitor);  %% old packet - stop broadcasting message
				false ->
					%% update info
					Net_data1 = add_connection(Box1,Port1,Port2,Box2,Net_data,Monitor),

					%% send message farther
					broadcast(Ports,Port,Links_pid,Pkt),
					box(Box,Ports,Links_pid,Net_data1,[TS|Arch],Monitor)
			end;
			

	%% Requests from network Emulator (Shell)

		{get_info,Pid} ->
			Info = lists:foldl( fun(B,Acc)-> [{B,neph:neighbors(B,Net_data)}|Acc]
								end,[],neph:box_list(Net_data)),
			Pid ! {network_info,Info},
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
			io:format("Boxes:~p~nWires:~p~n",[Boxes,Wires]),
			Pid ! {entire_net,Boxes,Wires},
			box(Box,Ports,Links_pid,Net_data,Arch,Pid)


	%	_ -> box(Box,Ports,Links_pid,Net_data,Arch,Monitor)
	end.




%% adds new connection to Net_data and sends update to Monitor
% returns new Net_data
add_connection(Box1,Port1,Port2,Box2,Net_data,undef) -> 
	neph:add_neighbor(Box1,Port1,Port2,Box2,Net_data);
add_connection(Box1,Port1,Port2,Box2,Net_data,Monitor) ->
	case neph:has_box(Box1,Net_data) of
		false -> Monitor ! {add_box, Box1};
		_ -> ok
	end,
	case neph:has_box(Box2,Net_data) of
		false -> Monitor ! {add_box, Box2};
		_ -> ok
	end,
	case neph:neighbor(Box1,Port1,Net_data) of
		{Port1,Port2,Box2} -> ok;
		_ -> Monitor ! {add_wire,Box1,Box2}
	end,
	neph:add_neighbor(Box1,Port1,Port2,Box2,Net_data).




%% sends {pkt,Port,Pkt} to to Links_pid for each port excepts a given Port_not_send
broadcast(Ports_to_send,Port_not_send,Links_pid,Pkt) ->						
	lists:foreach(  fun(P) ->
						case P of
							Port_not_send -> ok;
							_ -> Links_pid ! {pkt,P,Pkt}
						end
					end,Ports_to_send).




% sends out network info in a form of packets to a given port
% returns a new Archive of sent broadcasts
send_net_info(Box,Port,Links_pid,Net_data,Arch) ->
	io:format("broadcasting net info of ~p~n",[Box]),
	bni([Box],Port,Links_pid,Net_data,Arch,[]).

bni([Lead|Leads],Port,Links_pid,Net_data,Arch,Proc_boxes) ->
	Neibs = [ {P1,P2,B} || {P1,P2,B} <- neph:neighbors(Lead,Net_data), not lists:member(B,Proc_boxes)],
	Arch1 = lists:foldl(fun({P1,P2,B},Acc) ->
			TS = os:timestamp(),
			Pkt = {<<"FFFFFF">>,Port,<<"ND">>,{add_wire,Lead,P1,P2,B,TS}},
			io:format("broadcasting packet: ~p~n",[Pkt]),
			Links_pid ! {pkt,Port,Pkt },
			[TS|Acc] 
						end, Arch, Neibs),
	Boxes = lists:usort([ B || {_,_,B} <- Neibs]),
	bni(Boxes++Leads,Port,Links_pid,Net_data,Arch1,[Lead|Proc_boxes]);
bni([],_Port,_Links_pid,_Net_data,Arch,_Proc_boxes) -> Arch.




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

	io:format("Edges:~p~n",[Edges]),
	lists:foreach(  fun({B1,B2}) ->

		io:format(Dev,"\t~p -- ~p;~n",[B1,B2])
					end, Edges),

	io:format(Dev,"}~n",[]),
	file:close(Dev).


