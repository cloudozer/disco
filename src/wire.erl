% simulator of link discovery service in ivanOS
%
%

-module(wire).
-export([new/0, links/1, ask/2, free_ports/1, wires/1
		]).

-define(REQ_TIMEOUT,2000).



new() -> W = spawn(?MODULE,links,[dict:new()]), register(wh,W), W.


links(Connections) ->
	receive

	%% Messages from Port

		{pkt,Port,Pkt} -> 
			% check if port is wired.
			case dict:fetch(Port,Connections) of
				{not_connected,_,_} -> ok; % drop the packet
					%io:format("Packet dropped from port ~p - port not connected~n",[Port]);
				{Port2,_,_} ->
					{Port,ConnPort_pid,_} = dict:fetch(Port2,Connections),
					ConnPort_pid ! Pkt
			end,
			links(Connections);


		{new_port,Port,Port_pid,Box} -> 
			links(dict:store(Port,{not_connected,Port_pid,Box},Connections));


	%% Requests from Emulator

		{add_wire,Port1,Port2} ->
			Res1 = dict:fetch(Port1,Connections),
			Res2 = dict:fetch(Port2,Connections),

			case {Res1,Res2} of
				{{not_connected,P1_pid,B1},{not_connected,P2_pid,B2}} ->
					links(dict:store(Port2,{Port1,P2_pid,B2},dict:store(Port1,{Port2,P1_pid,B1},Connections)));
				_ ->
					io:format("Error: one or both ports is/are already connected~n"),
					links(Connections)
			end;

		{del_wire,Port1,Port2} ->
			case {dict:is_key(Port1,Connections), dict:is_key(Port2,Connections)} of
				{true,true} ->
					Res1 = dict:fetch(Port1,Connections),
					Res2 = dict:fetch(Port2,Connections),
					case {Res1,Res2} of
						{ {Port2,P1_pid,B1},{Port1,P2_pid,B2} } ->
							links(dict:store(Port2,{not_connected,P2_pid,B2},
								dict:store(Port1,{not_connected,P1_pid,B1},Connections)));
						_ ->
							io:format("ERROR: there is no wire connecting ~p port with ~p port~n",[Port1,Port2]),
							links(Connections)
					end;
				{_,_} -> 
					io:format("ERROR: either ~p port or ~p port not exist~n",[Port1,Port2]),
					links(Connections)
			end;

		{get_info,Pid} ->
			Pid ! {connections,dict:to_list(Connections)},
			links(Connections);

		{free_ports,Pid} ->
			Pid ! lists:sort([ {Box,Port} || {Port,{not_connected,_,Box}} <- dict:to_list(Connections) ]),
			links(Connections);

		{wires,Pid} ->
			{_,Wires} = 
			lists:foldl(fun({Port1,Port2},{Processed,Acc}) ->
				
							case lists:member(Port2,Processed) of
								true -> {[Port1|Processed],Acc};
								false-> {[Port1|Processed],[{Port1,Port2}|Acc]}
							end
						end,{[],[]},[ {Port1,Port2} || {Port1,{Port2,_,_}} <- dict:to_list(Connections),Port2 =/= not_connected]),

			Pid ! Wires,
			links(Connections);

		_ ->
			links(Connections)
	end.	



free_ports(W) -> 
	W ! {free_ports,self()},
	receive
		Answer -> Answer
	after
		?REQ_TIMEOUT -> no_response
	end.
	
wires(W) -> 
	W ! {wires,self()},
	receive
		Answer -> Answer
	after
		?REQ_TIMEOUT -> no_response
	end.


ask(Pid,Req_msg) ->
	Pid ! {Req_msg, self()},
	receive 
		Answer -> Answer 
	after
		?REQ_TIMEOUT -> no_response			
	end.


