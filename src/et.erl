% This module works with dict-based database located in a separate process
%
%

-module(et).
-export([
		fetch_keys/1, fetch/2, size/1, save_net/1
		]).




save_net(Net) ->
	Tid = ets:new(et,[]),
	lists:foreach(  fun(B)-> ets:insert(Tid,{B,dict:fetch(B,Net)})
					end,dict:fetch_keys(Net) ),
	Tid.



fetch(Box,Tid) -> [{Box,Neighbors}] = ets:lookup(Tid,Box), Neighbors.



fetch_keys(Tid) -> ets:foldl(  fun({B,_},Acc) -> [B|Acc]
							end,[],Tid).


size(Tid) -> 
	Info = ets:info(Tid), 
	{size, Size} = lists:keyfind(size,1,Info),
	Size.

