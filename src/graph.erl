% This module checks network connectivity
%
%

-module(graph).
-export([gen_rand_net/1,
		main/1,
		check_connectivity/0, check_connectivity/1
		]).

-define(PORTS_NBR,4).
-define(MAX_RAND_ATTEMPTS,5).



main(N) ->
	Net = gen_rand_net(N),
	{T1,Connected1} = timer:tc(?MODULE,check_connectivity,[Net]),
	case Connected1 of
		true -> io:format("Network connected entirely~n");
		false-> io:format("Network consists of a few disconnected subnetworks~n")
	end,
	io:format("connectivity checking took ~p us~n",[T1]),

	xs:save_net(Net),
	{T2,Connected2} = timer:tc(?MODULE,check_connectivity,[]),
	case Connected2 of
		true -> io:format("Network connected entirely~n");
		false-> io:format("Network consists of a few disconnected subnetworks~n")
	end,
	io:format("connectivity checking took ~p us~n",[T2]).
	



check_connectivity() -> % checks connectivity of xenstore database
	[Box|_] = xs:fetch_boxes(),
	Visited = sets:new(),
	check_connectivity([Box],Visited).

check_connectivity([Box|To_visit],Visited) ->	
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
	check_connectivity(To_visit1,sets:add_element(Box,Visited));
check_connectivity([],Visited) -> sets:size(Visited) =:= xs:size().
	


check_connectivity(Net) ->
	[Box|_] = dict:fetch_keys(Net),
	Visited = sets:new(),
	check_connectivity(Net,[Box],Visited).

check_connectivity(Net,[Box|To_visit],Visited) ->	
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
				end, To_visit, dict:fetch(Box,Net)),
	check_connectivity(Net,To_visit1,sets:add_element(Box,Visited));
check_connectivity(Net,[],Visited) -> sets:size(Visited) =:= dict:size(Net).
	



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
						{_,_} -> Boxes1 = lists:keyreplace(Box1,1,lists:keyreplace(Box2,1,Boxes,{Box2,Ports11}),{Box1,Ports11} )
					end,
					{{Box1,P1,P2,Box2}, Boxes1}
			end
	end.


