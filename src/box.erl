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
	lists:foreach(  fun(M) -> spawn(port,new, [M,Box,Bid,Links_pid]) 
					end, MACs).
	



box(Box,Ports,Links_pid) -> 
	box(Box,Ports,Links_pid,neph:new(Box),[]).

box(Box_id,Ports,Links_pid,Net_data,Arch) ->
	receive
		{new_connection, _My_port, _Neighbor_port,_Source_box } -> 
			box(Box_id,Ports,Links_pid,Net_data,Arch);

		{lost_connection, _My_port} ->
			box(Box_id,Ports,Links_pid,Net_data,Arch);

		{_Port,_Source_port,_Msg} ->
			% Check that Source_port is a neighbor port connected to your Port. If not drop a message
			% Check that neither Box1 nor Box2 is your Box. Drop otherwise

			box(Box_id,Ports,Links_pid,Net_data,Arch);

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


		{get_info,Pid} ->
			Pid ! {network_info,neph:boxe_list(Net_data)},
			box(Box_id,Ports,Links_pid,Net_data,Arch);

		{draw_net,_} ->
			to_dot(Box_id,Net_data),
			box(Box_id,Ports,Links_pid,Net_data,Arch);

		{ports_info,Pid} ->
			Pid ! {ports_info,Ports},
			box(Box_id,Ports,Links_pid,Net_data,Arch);

		_ -> box(Box_id,Ports,Links_pid,Net_data,Arch)

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

							end,{[],[]},neph:boxe_list(Net_data)),

	io:format("Edges:~p~n",[Edges]),
	lists:foreach(  fun({B1,B2}) ->

		io:format(Dev,"\t~p -- ~p;~n",[B1,B2])
					end, Edges),

	io:format(Dev,"}~n",[]),
	file:close(Dev).


