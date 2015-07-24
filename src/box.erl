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
	box(Box,Ports,Links_pid,neph:new(Box),[]).

box(Box,Ports,Links_pid,Net_data,Arch) ->
	receive

	%% Requests from my ports

		{new_connection, _My_port, _Neighbor_port,_Source_box } -> 
			box(Box,Ports,Links_pid,Net_data,Arch);

		{lost_connection, _My_port} ->
			box(Box,Ports,Links_pid,Net_data,Arch);

		{_Port,_Source_port,_Msg} ->
			% Check that Source_port is a neighbor port connected to your Port. If not drop a message
			% Check that neither Box1 nor Box2 is your Box. Drop otherwise

			box(Box,Ports,Links_pid,Net_data,Arch);

		%{Port,{<<"FFFFFF">>,Source_port,<<"ND">>,{add_wire,Box1,P1,P2,Box2,TS} } } ->
		%{Port,{<<"FFFFFF">>,Source_port,<<"ND">>,{del_wire,Box1,P1,P2,Box2,TS} } } ->




		
%		{pkt,Port,{<<"FFFFFF">>,_Source_port, {del_wire,Box1,P1,P2,Box2,TS}=Pkt} } ->
%			% check if this message has been already received
%			case lists:member(TS,Arch) of
%				true -> box(Box_id,Ports,Links_pid,Net_data,Arch);
%				false-> 
%					lists:foreach(  fun(P) -> Links_pid ! {pkt,P,{<<"FFFFFF">>,P,Pkt }}
%									end,lists:filter(fun(P)-> P=/=Port end,Ports)),
%					Net_data1 = neph:del_wire(Box1,P1,P2,Box2,Net_data),
%					box(Box_id,Ports,Links_pid,Net_data1,[TS|Arch])
%			end;
%
%		{pkt,Port,{<<"FFFFFF">>,_Source_port, {add_wire,Box1,P1,P2,Box2,TS}=Pkt} } ->
%			% check if this message has been already received
%			case lists:member(TS,Arch) of
%				true -> box(Box_id,Ports,Links_pid,Net_data,Arch);
%				false-> 
%					lists:foreach(  fun(P) -> Links_pid ! {pkt,P,{<<"FFFFFF">>,P,Pkt }}
%									end,lists:filter(fun(P)-> P=/=Port end,Ports)),
%					Net_data1 = neph:add_neighbor(Box1,P1,P2,Box2,Net_data),
%					box(Box_id,Ports,Links_pid,Net_data1,[TS|Arch])
%			end;

	%% Requests from network Emulator (Shell)

		{get_info,Pid} ->
			Pid ! {network_info,neph:box_list(Net_data)},
			box(Box,Ports,Links_pid,Net_data,Arch);

		{draw_net,_} ->
			to_dot(Box,Net_data),
			box(Box,Ports,Links_pid,Net_data,Arch);

		{ports_info,Pid} ->
			Pid ! {ports_info,Ports},
			box(Box,Ports,Links_pid,Net_data,Arch);

	%% Requests from Web Monitor

		{get_entire_net, Pid} -> 
			io:format("Got message from WS~n"),
			{Boxes,Wires} = neph:box_wire_list(Net_data),
			io:format("Boxes:~p~nWires:~p~n",[Boxes,Wires]),
			Pid ! {entire_net,Boxes,Wires},
			box(Box,Ports,Links_pid,Net_data,Arch);

		_ -> box(Box,Ports,Links_pid,Net_data,Arch)

	end.


% sends out network info in a form of packets to a given port
% returns a new Archive of sent broadcasts
broadcast_net_info(Box,Port,Links_pid,Net_data,Arch) ->
	bni([Box],Port,Links_pid,Net_data,Arch,[]).

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


