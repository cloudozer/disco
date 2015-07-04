% simulator of link discovery service in ivanOS
%
%


-module(wire).
-export([new/2
		]).


new(Control_pid,Wire_id) ->
	receive
		quit -> ok;
		{plugged,Pid1,Port1} -> new(Control_pid,Wire_id,Pid1,Port1)
	end.

new(Control_pid,Wire_id,Pid1,Port1) ->
	receive
		quit -> ok;
		{plugged,Pid2,Port2} -> wire(Control_pid,Wire_id,Pid1,Port1,Pid2,Port2)
	end.

wire(Control_pid,Wire_id,Pid1,Port1,Pid2,Port2) ->
	receive
		quit -> ok;
		Msg={Dest,Port1,_,_} when Dest =:= Port2; Dest =:= <<"FFFFFF">> -> 
			Pid2 ! Msg, 
			wire(Control_pid,Wire_id,Pid1,Port1,Pid2,Port2); 
		Msg={Dest,Port2,_,_} when Dest =:= Port1; Dest =:= <<"FFFFFF">> -> 
			Pid1 ! Msg, 
			wire(Control_pid,Wire_id,Pid1,Port1,Pid2,Port2); 
		Msg -> 
			io:format("Wire: ~p drops packet: ~p~n",[Wire_id, Msg]),
			wire(Control_pid,Wire_id,Pid1,Port1,Pid2,Port2)
	end.