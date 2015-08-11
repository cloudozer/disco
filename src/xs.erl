% This module works with xenstore database
%
%

-module(xs).
-export([
		fetch_boxes/0, fetch/1, size/0,
		save_net/1,
		pp/1
		]).

-define(HOME,"data/network-discovery").



fetch_boxes() ->
	case xenstore:list(?HOME) of
		{ok,Ls} -> Ls;
		{error,Err} -> error(Err)
	end.


fetch(Box1) ->
	case xenstore:list(?HOME++"/"++Box1) of
		{ok,Ls} -> 
			Ports = [ P || [_,_,$-,_,_,$-,_,_,$-,_,_,$-,_,_,$-,_,_]=P <- Ls ],
			lists:foldl(fun(P,Acc)-> 
						{ok,Link} = xenstore:read(?HOME++"/"++Box1++"/"++P),
						[{P,lists:sublist(Link,17),lists:sublist(Link,19,17)}|Acc]
						end,[],Ports);
		{error,Err} -> error(Err)
	end.


size() -> length(fetch_boxes()).



save_net(Net) ->
	xenstore:delete(?HOME),
	xenstore:write(?HOME,""),
	lists:foreach(   fun(Box) ->
					xenstore:write(?HOME++"/"++pp(Box),""),
					Links = dict:fetch(Box,Net),
					lists:foreach(  fun({P1,P2,Box2}) -> 
									xenstore:write(?HOME++"/"++pp(Box)++"/"++pp(P1),pp(P2)++"|"++pp(Box2))
									end, Links),
					xenstore:write(?HOME++"/"++pp(Box)++"/cores",8),
					xenstore:write(?HOME++"/"++pp(Box)++"/RAM",32),
					xenstore:write(?HOME++"/"++pp(Box)++"/Free_mem",24)
					end, dict:fetch_keys(Net) ).



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




