% simulator of link discovery service in ivanOS
%
%


-module(wire).
-export([new/0, links/1
		]).


new() -> spawn(?MODULE,links,[dict:new()]).


links(Connections) ->
	receive
		{new_port,Port} -> 
			links(dict:store(Port,not_connected,Connections));

		{get_info,Pid} ->
			Pid ! Connections,
			links(Connections);

		{add_wire,Port1,Port2} ->
			not_connected = dict:fetch(Port1,Connections),
			not_connected = dict:fetch(Port2,Connections),
			links(dict:store(Port2,Port1,dict:store(Port1,Port2,Connections)));

		_ ->
			links(Connections)
	end.	


