% simulator of link discovery service in ivanOS
%
%


-module(wire).
-export([new/0, links/1
		]).


new() -> spawn(?MODULE,links,[dict:new()]).


links(Connections) ->
	receive
		{pkt,Port,Pkt} -> 
			% check if port is wired.
			case dict:fetch(Port,Connections) of
				{not_connected,_} -> ok; % drop the packet
					%io:format("Packet dropped from port ~p - port not connected~n",[Port]);
				{Port2,_} ->
					{Port,Box2_pid} = dict:fetch(Port2,Connections),
					Box2_pid ! {pkt,Port2,Pkt}
			end,
			links(Connections);


		{new_port,Port,Box_pid} -> 
			links(dict:store(Port,{not_connected,Box_pid},Connections));

		{get_info,Pid} ->
			Pid ! {connections,dict:to_list(Connections)},
			links(Connections);

		{add_wire,Port1,Port2} ->
			{not_connected,B1} = dict:fetch(Port1,Connections),
			{not_connected,B2} = dict:fetch(Port2,Connections),
			links(dict:store(Port2,{Port1,B2},dict:store(Port1,{Port2,B1},Connections)));

		{del_wire,Port1,Port2} ->
			{Port2,B1} = dict:fetch(Port1,Connections),
			{Port1,B2} = dict:fetch(Port2,Connections),
			links(dict:store(Port2,{not_connected,B2},dict:store(Port1,{not_connected,B1},Connections)));

		_ ->
			links(Connections)
	end.	


