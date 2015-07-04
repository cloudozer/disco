% simulator of link discovery service in ivanOS
%
%


-module(main).
-export([log/1, run/0, get_mac/0]).


run() ->
	Log_pid = spawn(?MODULE,log,[ self() ]),

	Box_ids = [101,102,103,104],
	Wire_ids = [1,2,3,4],
	Network = [{1,101,102},{2,102,103},{3,101,103},{4,102,104}],  %% format: {Wire,Box1,Box2}
	make_dot("network_topology.dot",Network),
	
	Boxes = spawn_boxes(Log_pid,Box_ids),
	Wires = spawn_wires(Log_pid,Wire_ids),
	Box_wires_dict = wire_boxes(Network),

	lists:foreach(fun(Box) ->
		Wire_list = dict:fetch(Box,Box_wires_dict),
		{Box,Box_pid} = lists:keyfind(Box,1,Boxes),
		Wire_pids = [ begin {W,Wire_pid} = lists:keyfind(W,1,Wires), Wire_pid end || W <- Wire_list ],
		Box_pid ! Wire_pids,
		timer:sleep(random:uniform(50))				
							
				  end, dict:fetch_keys(Box_wires_dict)),
	
	receive
		Log -> 
			[ begin 
				Pid ! quit, 
				timer:sleep(50) 
			end || Pid <- [B||{_,B}<-Boxes]++[W||{_,W}<-Wires] ],
			Log
	end.




wire_boxes(Network) -> find_wires(Network,dict:new()).

find_wires([{W,B1,B2}|Ls],Dict) ->
	find_wires(Ls,dict:append(B2,W,dict:append(B1,W,Dict)));
find_wires([],Dict) ->
	io:format("Network:~p~n",[ [{K,dict:fetch(K,Dict)} || K<-dict:fetch_keys(Dict)] ]),
	Dict.


spawn_boxes(Log_pid,Ls) -> lists:foldl(fun(ID,Acc)-> [{ID,spawn(box,new,[Log_pid,ID])}|Acc] 
								end,[],Ls).

spawn_wires(Log_pid,Ls) -> lists:foldl(fun(ID,Acc)-> [{ID,spawn(wire,new,[Log_pid,ID])}|Acc] 
								end,[],Ls).



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
	



make_dot(File,Network) ->
	{ok,Dev} = file:open(File,write),

	io:format(Dev,"graph Network {~n\tnode [shape=box,style=filled,color=grey];~n",[]),

	lists:foreach(  fun({W,B1,B2}) ->
		io:format(Dev,"\t~p -- ~p [label=~p];~n",[B1,B2,W])
					end, Network),

	io:format(Dev,"}~n",[]),
	file:close(Dev).


