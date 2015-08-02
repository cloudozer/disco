% simulator of link discovery service in ivanOS
%
%

-module(nd).
-export([run/1,
		verify/2,
		to_dot/1
		]).

-define(NBR_PORTS,4).
-define(WIRING_DELAY,500).  % deleay in msec before wiring next boxes
-define(MAX_RAND_ATTEMPTS,10).


% runs a simulation for a random graph having N boxes
% each box has 4 ports, Number of wires is between (N .. 2xN)
run(N) ->
	W = wire:new(),

	%% generate N boxes
	lists:foreach( fun(J) -> spawn(box,new,[box_name(J),?NBR_PORTS,W])
					end, lists:seq(1,N) ),

	%% generate edges and write a dot file
	crypto:start(),
	M = crypto:rand_uniform(round(1.6*N), 2*N),
	timer:sleep(2000),
	Net = dict:new(),
	Net1 = wire_boxes(W,M,Net),
	to_dot(Net1),
	Net1.




verify(G,Net_data) ->
	io:format("Verification started * * * * * * * *~n"),
	N1 = dict:size(G),
	N2 = neph:size(Net_data),

	case N1 =:= N2 of
		true -> io:format("Both graphs has the same number of nodes: ~p~n",[N1]);
		false-> io:format("The real network consists of ~p boxes while~nbox1 discovered ~p boxes~n",[N1,N2])
	end,

	lists:foreach(fun(B) -> 
					case neph:has_box(B,Net_data) of
						false -> 
							io:format("~p was not discovered along with its wires:~n~p~n",
								[B,dict:fetch(B,G)]);
						true ->
							Golden = lists:sort(dict:fetch(B,G)),
							Discovered = lists:sort(neph:neighbor_boxes(B,Net_data)),
							case Golden == Discovered of
								true -> ok;
								false->
									io:format("Neighbors of ~p do not conform:~n",[B]),
									io:format("  Ethalon: ~p~n  Discovered: ~p~n~n",[Golden,Discovered])
							end
					end
				end,dict:fetch_keys(G)),

	Redundant = sets:subtract(sets:from_list(neph:box_list(Net_data)),sets:from_list(dict:fetch_keys(G))),
	lists:foreach(  fun(B) -> io: format("~p should not been descovered~n",[B])
					end,sets:to_list(Redundant)).






to_dot(Net) ->
	{ok,Dev} = file:open("Net.dot",write),

	io:format(Dev,"graph IvanCloud {~n\tnode [shape=box,style=filled,color=grey];~n",[]),
	io:format(Dev,"\t ~p [color=yellow];~n",[box1]),
	{Edges,_} = lists:foldl(fun(B,{Acc,Processed_boxes}) -> 
		Neibs = [ Nb || Nb <- dict:fetch(B,Net), not lists:member(Nb,Processed_boxes) ],
		{[ {Nb,B} || Nb <- Neibs]++Acc, [B|Processed_boxes]}
							end,{[],[]},dict:fetch_keys(Net)),
	to_dot(Edges,Dev).

to_dot(Edges,Dev) ->
	lists:foreach(  fun({B1,B2}) -> io:format(Dev,"\t~p -- ~p;~n",[B1,B2])
					end, Edges),
	io:format(Dev,"}~n",[]),
	file:close(Dev).



wire_boxes(_W,0,Net) -> Net;
wire_boxes(W,M,Net) -> 
	Ports = wire:free_ports(W),
	case get_random_wire(Ports,0) of
		{Box1,P1,P2,Box2} ->
			W ! {add_wire,P1,P2},
			Net1 = dict:append(Box2,Box1,dict:append(Box1,Box2,Net) ),
			timer:sleep(?WIRING_DELAY),
			wire_boxes(W,M-1,Net1);
		false ->
			io:format("~p wires could not wire~n",[M]),
			wire_boxes(W,0,Net)
	end.

	


get_random_wire(_,?MAX_RAND_ATTEMPTS) -> false;
get_random_wire(Ports,K) ->
	N = length(Ports),
	case N < 2 of
		true -> false;
		false ->
			J1 = random:uniform(N),
			{Box1,P1} = lists:nth(J1,Ports),
			J2 = random:uniform(N),
			{Box2,P2} = lists:nth(J2,Ports),
			
			case Box1 =:= Box2 of
				true -> get_random_wire(Ports,K+1);
				false-> {Box1,P1,P2,Box2}
			end
	end.



box_name(J) -> list_to_atom("box"++integer_to_list(J) ).

