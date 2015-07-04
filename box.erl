% simulator of link discovery service in ivanOS
%
%


-module(box).
-export([new/2,
		port/5,
		box/4,
		port/1
		]).



new(Control_pid,Box_id) ->
	Network = dict:store(Box_id,[],dict:new()),

	receive
		Links -> 
			Port_pids = [ begin
							timer:sleep(random:uniform(100)),				
							Link_pid = lists:nth(J,Links),
							Port_mac = main:get_mac(),
							Port_pid = spawn(?MODULE,port,[Control_pid,self(),Box_id,Port_mac,Link_pid]),
							Link_pid ! {plugged,Port_pid,Port_mac},
							Port_pid
						  end || J <- lists:seq(1,length(Links)) ],
			
			box(Control_pid,Box_id,Network,Port_pids)
	end.



box(Control_pid,Box_id,Network,Ports) -> 
	receive
		quit -> 
			box_info_to_dot(Box_id,Network),
			[ {self(),quit} || Port_pid <- Ports];

		{Port_pid, ping_resp, {Dest,Source,_,{ping_resp,Source_box_id}}} -> 
			% check if it is already known
			Neigbors = dict:fetch(Box_id,Network),
			case lists:keyfind(Dest,1,Neigbors) of
				false -> 							%% was not connected before
					Network1 = dict:append(Box_id,{Dest,Source,Source_box_id},Network),
					box(Control_pid,Box_id,Network1,Ports);

				{Dest,Source,Source_box_id} ->		 	%% connection is known 
					box(Control_pid,Box_id,Network,Ports);

				{Dest,New_source,Other_box_id} ->		%% connection is new
					Neighbors1 = lists:keydelete(Dest,1,Neigbors),
					Network1 = dict:store(Box_id,Neighbors1,Network),
					io:format("Connection changed~n"),
					box(Control_pid,Box_id,Network1,Ports)

			end
	end.




port(Control_pid,Box_pid, Box_id, Port_mac, Link_pid) -> 
	receive
		{_, quit} -> ok;

		Msg={_Dest,Source,EthType,{ping,Source_box_id}} ->
			log(Control_pid,rcv,Port_mac,Msg),
			Link_pid ! {Source,Port_mac,EthType,{ping_resp,Box_id}},  		%% send ping back
			port(Control_pid,Box_pid, Box_id, Port_mac, Link_pid);

		Msg={_Dest,_Source,_,{ping_resp,Source_box_id}} ->
			log(Control_pid,rcv,Port_mac,Msg),
			Box_pid ! {self(),ping_resp,Msg},							%% send msg to box to check it
			port(Control_pid,Box_pid, Box_id, Port_mac, Link_pid)


	after
		100 ->
			Link_pid ! Msg={<<"FFFFFF">>,Port_mac,<<"CC">>,{ping,Box_id}},
			log(Control_pid,snd,Port_mac,Msg),
			port(Control_pid,Box_pid, Box_id, Port_mac, Link_pid)
	end.



print_box_info(Box_id,Network) ->
	io:format("Box ~p info:~n",[Box_id]),
	lists:foreach(  fun({P1,P2,B}) -> 
		io:format("\tport ~s --//-- port ~s box: ~p~n",[port(P1),port(P2),B])
					end,dict:fetch(Box_id,Network)).


box_info_to_dot(Box_id,Network) ->
	File = integer_to_list(Box_id)++".dot",
	{ok,Dev} = file:open(File,write),

	io:format(Dev,"graph ~p {~n\tnode [shape=box];~n",[Box_id]),
	io:format(Dev,"\t~p [filled=true,color=yellow];~n",[Box_id]),

	lists:foreach(  fun(B1) ->
		Connections = dict:fetch(B1,Network),
		lists:foreach(  fun({_,_,B2}) ->
			io:format(Dev,"\t~p -- ~p ;~n",[B1,B2]) 
						end,Connections)
		
					end, dict:fetch_keys(Network)),

	io:format(Dev,"}~n",[]),
	file:close(Dev).




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


log(Control_pid,Type,Port_mac,Msg) -> Control_pid ! {os:timestamp(),Port_mac,Type,Msg}.



