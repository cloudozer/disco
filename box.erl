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
		Ports -> 
			Link_pids = [spawn(?MODULE,port,[Control_pid,self(),Box_id,J,lists:nth(J,Ports)]) || J <- lists:seq(1,length(Ports)) ],
			box(Control_pid,Box_id,Net_dict,Link_pids),
			io:format("Box: ~p started~n",[Box_id])
	end.



box(Control_pid,Box_id,Net_dict,Ports) -> 
	receive
		{Control_pid, quit} -> [ {self(),quit} || Port_pid <- Ports]

	end.




port(Control_pid,Box_pid, Box_id, Port_id, Link_pid) -> 
	receive
		{Box_pid, quit} -> ok;

		Msg={ping,Port_from,Box_from} -> 
			log(Control_pid,rcv,Link_pid,Msg),
			Link_pid ! {ping_resp,Port_id,Box_id},
			port(Control_pid,Box_pid, Box_id, Port_id, Link_pid);

		Msg={ping_resp, Port_from, Box_from} ->
			log(Control_pid,rcv,Link_pid,Msg),
			port(Control_pid,Box_pid, Box_id, Port_id, Link_pid)

	after
		1000 ->
			Link_pid ! Msg={ping,Port_id,Box_id},
			log(Control_pid,snd,Link_pid,Msg),
			port(Control_pid,Box_pid, Box_id, Port_id, Link_pid)
	end.




log(Control_pid,Type,Pid,Msg) -> Control_pid ! {Pid,Type,Msg}.



