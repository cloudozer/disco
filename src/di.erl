% This module works with dict-based database located in a separate process
%
%

-module(di).
-export([
		fetch_keys/0, fetch/1, size/0, save_net/1,
		net_info/1
		]).


net_info(Net) ->
	receive
		{get_box_conn,Pid,Box} -> Pid ! dict:fetch(Box,Net), net_info(Net);

		{get_boxes,Pid} -> Pid ! dict:fetch_keys(Net), net_info(Net)

	after
		5000 -> ok
	end.



save_net(Net) ->
	Pid = spawn(?MODULE,net_info,[Net]),
	register(net,Pid).



fetch(Box) ->
	net ! {get_box_conn, self(), Box},
	receive Links -> Links after 2000 -> error(timeout) end.


fetch_keys() ->
	net ! {get_boxes,self()},
	receive Boxes -> Boxes after 2000 -> error(timeout) end.


size() ->
	length( fetch_keys() ).	


