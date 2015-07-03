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
							Port_pid = spawn(?MODULE,port,[Control_pid,self(),Box_id,J,Link_pid]),
							Link_pid ! {plugged,Port_pid,J},
							Port_pid
						  end || J <- lists:seq(1,length(Links)) ],
			box(Control_pid,Box_id,Net_dict,Port_pids)
	end.



box(Control_pid,Box_id,Net_dict,Ports) -> 
	receive
		{Control_pid, quit} -> [ {self(),quit} || Port_pid <- Ports]

	end.




port(Control_pid,Box_pid, Box_id, Port_id, Link_pid) -> 
	receive
		{Box_pid, quit} -> ok;

		Msg={Port_from,{ping,Box_from}} -> 
			log(Control_pid,rcv,Link_pid,Msg),
			Link_pid ! {Port_id,{ping_resp,Box_id}},
			port(Control_pid,Box_pid, Box_id, Port_id, Link_pid);

		Msg={Port_from, {ping_resp, Box_from}} ->
			log(Control_pid,rcv,Link_pid,Msg),
			port(Control_pid,Box_pid, Box_id, Port_id, Link_pid)

	after
		1000 ->
			Link_pid ! Msg={Port_id,{ping,Box_id}},
			log(Control_pid,snd,Link_pid,Msg),
			port(Control_pid,Box_pid, Box_id, Port_id, Link_pid)
	end.




log(Control_pid,Type,Pid,Msg) -> Control_pid ! {Pid,Type,Msg}.



