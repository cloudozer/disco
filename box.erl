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

		Msg={Port_from,{ping,Box_from}} -> 
			log(Control_pid,rcv,Port_mac,Msg),
			Link_pid ! {Port_mac,{ping_resp,Box_id}},
			port(Control_pid,Box_pid, Box_id, Port_mac, Link_pid);

		Msg={Port_from, {ping_resp, Box_from}} ->
			log(Control_pid,rcv,Port_mac,Msg),
			port(Control_pid,Box_pid, Box_id, Port_mac, Link_pid)

	after
		100 ->
			Link_pid ! Msg={Port_mac,{ping,Box_id}},
			log(Control_pid,snd,Port_mac,Msg),
			port(Control_pid,Box_pid, Box_id, Port_mac, Link_pid)
	end.




log(Control_pid,Type,Port_mac,Msg) -> Control_pid ! {os:timestamp(),Port_mac,Type,Msg}.



