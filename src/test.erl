% simulator of link discovery service in ivanOS
%
%

-module(test).
-export([
		t/0,
		ask/2
		]).

-define(REQ_TIMEOUT,2000).


t() -> ok.




ask(Pid,Req_msg) ->
	Pid ! {Req_msg, self()},
	receive 
		Answer -> Answer 
	after
		?REQ_TIMEOUT -> no_response			
	end.