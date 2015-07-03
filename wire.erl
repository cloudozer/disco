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
		{Port1,Data} -> Pid2 ! {Port1,Data}, wire(Control_pid,Wire_id,Pid1,Port1,Pid2,Port2); 
		{Port2,Data} -> Pid1 ! {Port2,Data}, wire(Control_pid,Wire_id,Pid1,Port1,Pid2,Port2)
	end.