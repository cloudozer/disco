% This module works with xenstore database
%
%

-module(xs).
-export([
		save_net/1,
		pp/1
		]).






save_net(Net) ->
	Home = "data/network-discovery",
	xenstore:write(Home,""),
	lists:foreach(   fun(Box) ->
					xenstore:write(Home++"/"++pp(Box),""),
					Links = dict:fetch(Box,Net),
					lists:foreach(  fun({P1,P2,Box2}) -> 
									xenstore:write(Home++"/"++pp(Box)++"/"++pp(P1),pp(P2)++"|"++pp(Box2))
									end, Links),
					xenstore:write(Home++"/"++pp(Box)++"/cores",8),
					xenstore:write(Home++"/"++pp(Box)++"/RAM",32),
					xenstore:write(Home++"/"++pp(Box)++"/Free_mem",24)
					end,dict:fetch_keys(Net) ).



pp(<<P1,P2,P3,P4,P5,P6>>) -> 
	byte_to_hex(P1)++"-"++byte_to_hex(P2)++"-"++
	byte_to_hex(P3)++"-"++byte_to_hex(P4)++"-"++
	byte_to_hex(P5)++"-"++byte_to_hex(P6). 




byte_to_hex(B) ->
	S = integer_to_list(B,16),
	case length(S) of
		2 -> S;
		1 -> [$0|S]
	end.




