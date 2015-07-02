% simulator of link discovery service in ivanOS
%
%


-module(wire).
-export([new/2
		]).


new(Control_pid,Wire_id) ->
	
	receive
		Ports -> 
			Link_pids = [spawn(?MODULE,port,[Control_pid,self(),Box_id,J,lists:nth(J,Ports)]) || J <- lists:seq(1,length(Ports)) ],
			box(Control_pid,Box_id,Net_dict,Link_pids),
			io:format("Box: ~p started~n",[Box_id])
	end.