% simulator of link discovery service in ivanOS
%
%


-module(main).
-export([log/1, run/0]).


run() ->
	Box_id = 222,
	B = spawn(box,new,[self(),Box_id]),
	B ! [11,12],

	spawn(?MODULE,log,[[]]).




log(Log) ->
	receive
		quit -> io:format("Log:~n~p~n",[Log]);
		Msg -> log([Msg|Log])
	end.
