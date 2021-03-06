% This module checks network connectivity
%
%

-module(graph).
-export([gen_rand_net/1,
		main/1,
		check_conn_xs/0, check_conn_di/0, check_conn_et/1
		]).

-define(PORTS_NBR,4).
-define(MAX_RAND_ATTEMPTS,5).



main(N) ->
	Net = gen_rand_net(N),
	di:save_net(Net),
	{T1,Connected1} = timer:tc(?MODULE,check_conn_di,[]),
	case Connected1 of
		true -> io:format("Network connected entirely~n");
		false-> io:format("Network consists of a few disconnected subnetworks~n")
	end,
	io:format("Dict connectivity checking took ~p us~n",[T1]),

	Tid = et:save_net(Net),
	{T2,Connected2} = timer:tc(?MODULE,check_conn_et,[Tid]),
	case Connected2 of
		true -> io:format("Network connected entirely~n");
		false-> io:format("Network consists of a few disconnected subnetworks~n")
	end,
	io:format("ETS connectivity checking took ~p us~n",[T2]).
	



check_conn_xs() -> % checks connectivity of xenstore database
	[Box|_] = xs:fetch_boxes(),
	Visited = sets:new(),
	check_conn_xs([Box],Visited).

check_conn_xs([Box|To_visit],Visited) ->	
	To_visit1 = 
	lists:foldl(fun({_,_,B},Acc) -> 
				case sets:is_element(B,Visited) of
					true -> Acc;
					false-> 
						case lists:member(B,Acc) of
							false->[B|Acc];
							true -> Acc
						end
				end
				end, To_visit, xs:fetch(Box)),
	check_conn_xs(To_visit1,sets:add_element(Box,Visited));
check_conn_xs([],Visited) -> sets:size(Visited) =:= xs:size().
	


check_conn_di() ->
	[Box|_] = di:fetch_keys(),
	Visited = sets:new(),
	check_conn_di([Box],Visited).

check_conn_di([Box|To_visit],Visited) ->	
	To_visit1 = 
	lists:foldl(fun({_,_,B},Acc) -> 
				case sets:is_element(B,Visited) of
					true -> Acc;
					false-> 
						case lists:member(B,Acc) of
							false->[B|Acc];
							true -> Acc
						end
				end
				end, To_visit, di:fetch(Box)),
	check_conn_di(To_visit1,sets:add_element(Box,Visited));
check_conn_di([],Visited) -> sets:size(Visited) =:= di:size().
	


check_conn_et(Tid) ->
	[Box|_] = et:fetch_keys(Tid),
	Visited = sets:new(),
	check_conn_et(Tid,[Box],Visited).

check_conn_et(Tid,[Box|To_visit],Visited) ->	
	To_visit1 = 
	lists:foldl(fun({_,_,B},Acc) -> 
				case sets:is_element(B,Visited) of
					true -> Acc;
					false-> 
						case lists:member(B,Acc) of
							false->[B|Acc];
							true -> Acc
						end
				end
				end, To_visit, et:fetch(Box,Tid)),
	check_conn_et(Tid,To_visit1,sets:add_element(Box,Visited));
check_conn_et(Tid,[],Visited) -> sets:size(Visited) =:= et:size(Tid).



gen_rand_net(0) -> ok;
gen_rand_net(N) ->
	crypto:start(),
	M = round(0.3 *?PORTS_NBR*N + random:uniform(round(0.2*?PORTS_NBR*N))),
	io:format("Generating ~w wires~n",[M]),

	Boxes = lists:foldl(fun(_,Acc)-> 
				[K|_] = MACs = [ port:get_mac() || _ <- lists:seq(1,?PORTS_NBR) ],
				[{K,MACs}|Acc]
						end,[],lists:seq(1,N)),

	wire_boxes(M,dict:new(),Boxes).



wire_boxes(0,Net,_) -> Net;
wire_boxes(M,Net,Boxes) ->
	case get_random_wire(Boxes,0) of
		{{Box1,P1,P2,Box2}, Boxes1} ->
			Net1 = dict:append(Box1,{P1,P2,Box2},dict:append(Box2,{P2,P1,Box1},Net)),
			wire_boxes(M-1,Net1,Boxes1);
		false ->
			io:format("~p wires could not be used~n",[M]),
			wire_boxes(0,Net,Boxes)
	end.



get_random_wire(_,?MAX_RAND_ATTEMPTS) -> false;
get_random_wire(Boxes,K) ->
	N = length(Boxes),
	case N < 2 of
		true -> false;
		false ->
			J1 = random:uniform(N),
			{Box1,Ports1} = lists:nth(J1,Boxes),
			J2 = random:uniform(N),
			{Box2,Ports2} = lists:nth(J2,Boxes),
			
			case Box1 =:= Box2 of
				true -> get_random_wire(Boxes,K+1);
				false-> 
					[P1|Ports11] = Ports1,
					[P2|Ports22] = Ports2,
					case {Ports11,Ports22} of
						{[],[]} -> Boxes1 = lists:keydelete(Box2,1,lists:keydelete(Box1,1,Boxes) );
						{[],_} -> Boxes1 = lists:keyreplace(Box2,1,lists:keydelete(Box1,1,Boxes),{Box2,Ports22} );
						{_,[]} -> Boxes1 = lists:keyreplace(Box1,1,lists:keydelete(Box2,1,Boxes),{Box1,Ports11} );
						{_,_} -> Boxes1 = lists:keyreplace(Box1,1,lists:keyreplace(Box2,1,Boxes,{Box2,Ports22}),{Box1,Ports11} )
					end,
					{{Box1,P1,P2,Box2}, Boxes1}
			end
	end.


