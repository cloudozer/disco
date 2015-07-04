% simulator of link discovery service in ivanOS
%
%


-module(box).
-export([new/2,
		port/5,
		box/4
		]).



new(Control_pid,Box_id) ->
	Net_dict = dict:store(Box_id,[],dict:new()),

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
			
			box(Control_pid,Box_id,Net_dict,Port_pids)
	end.



box(Control_pid,Box_id,Net_dict,Ports) -> 
	receive
		{Control_pid, quit} -> [ {self(),quit} || Port_pid <- Ports]

	end.




port(Control_pid,Box_pid, Box_id, Port_mac, Link_pid) -> 
	receive
		{Box_pid, quit} -> ok;

		Msg={Dest,Source,EthType,ping} ->
			log(Control_pid,rcv,Port_mac,Msg),
			Link_pid ! {Source,Port_mac,EthType,ping_resp},
			port(Control_pid,Box_pid, Box_id, Port_mac, Link_pid);


		Msg={Dest,Source,EthType,ping_resp} ->
			log(Control_pid,rcv,Port_mac,Msg),
			port(Control_pid,Box_pid, Box_id, Port_mac, Link_pid)

	after
		100 ->
			Link_pid ! Msg={<<"FFFFFF">>,Port_mac,<<"CC">>,ping},
			log(Control_pid,snd,Port_mac,Msg),
			port(Control_pid,Box_pid, Box_id, Port_mac, Link_pid)
	end.




log(Control_pid,Type,Port_mac,Msg) -> Control_pid ! {os:timestamp(),Port_mac,Type,Msg}.



