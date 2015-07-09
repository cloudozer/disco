% simulator of link discovery service in ivanOS
%
%


-module(test).
-export([
		t/0,
		ask/2
		]).



t() -> ok.




ask(Pid,Req_msg) ->
	Pid ! {Req_msg, self()},
	receive Answer -> Answer end.