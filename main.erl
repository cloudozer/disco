% simulator of link discovery service in ivanOS
%
%


-module(main).
-export([log/1, run/0, get_mac/0]).


run() ->
	Log_pid = spawn(?MODULE,log,[ self() ]),

	B1 = spawn(box,new,[Log_pid,101]),
	B2 = spawn(box,new,[Log_pid,102]),
	B3 = spawn(box,new,[Log_pid,103]),

	L1 = spawn(wire,new,[Log_pid,1]),
	L2 = spawn(wire,new,[Log_pid,2]),

	%% B1 - L1 - B2 - L2 - B3
	B1 ! [L1],
	B2 ! [L1,L2],
	B3 ! [L2],

	receive
		Log -> 
			[ Pid ! quit || Pid <- [B1,B2,B3,L1,L2] ],
			Log
	end.



log(Pid) -> log(Pid,[]).
log(Pid,Log) ->
	receive
		Msg ->
			%io:format("Message:~p~n",[Msg]), 
			case length(Log) > 20 of
				true -> Pid ! [Msg|Log];
				false-> log(Pid,[Msg|Log])
			end
	end.



get_mac() -> crypto:strong_rand_bytes(6).
	

