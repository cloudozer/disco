%
% simple graph library 
% developed to store network information
%

% graph is a tuple containing two dictionaries and a list: 
% one dict contains node-list of neighbors key-values 
% another dict contains pairs of {box1,box2}-list of links that connects boxes

-module(neph).
-export([new/1,size/1,
		has_box/2,
		boxes/1,
		neighbor/3,
		neighbors/2,
		add_neighbor/5,
		del_wire/5
		%export_to_dot/2
		]).


% creates new network
new(Box) -> { dict:store(Box,[],dict:new()), dict:new()}.  % {Network,Wires,Ports}

% returns size of known network
size({Network,_}) -> dict:size(Network).


% returns true/false if Graph has/has_not Node  
has_box(Box,{Network,_}) -> dict:is_key(Box,Network).
	

% returns the all boxes in the network (known for a given box)
boxes({Network,_}) -> dict:fetch_keys(Network).


% returns {Neighbor_port, Neighbor_box} or not_connected
neighbor(Box_id,Port,{Network,_}) ->
	Neighbors = dict:fetch(Box_id,Network),
	case lists:keyfind(Port,1,Neighbors) of
		false -> not_connected;
		{Port,Nei_port,Nei_box} -> {Nei_port,Nei_box}
	end.


% returns all neighbor boxes for a given box
neighbors(Box_id,{Network,_}) ->
	[ B || {_,_,B} <- dict:fetch(Box_id,Network)].


% returns Net_data1 after removing a connection between boxes
del_wire(Box1,Port1,Port2,Box2,{Network,Links}) ->
	Neighbors1 = dict:fetch(Box1,Network),
	Neighbors11=lists:keydelete(Port1,1,Neighbors1),
	
	Neighbors2 = dict:fetch(Box2,Network),
	Neighbors22=lists:keydelete(Port2,1,Neighbors2),

	{dict:store(Box2,Neighbors22,dict:store(Box1,Neighbors11,Network)), Links}.



% returns updated Net_data after adding new box connected over given ports
add_neighbor(Box1,Port1,Port2,Box2,{Network,Links}) ->
	Network1 = dict:append(Box2,{Port2,Port1,Box1},dict:append(Box1,{Port1,Port2,Box2},Network)),
	Links1 = dict:append({Box2,Box1},{Port2,Port1},dict:append({Box1,Box2},{Port1,Port2},Links)),
	{Network1,Links1}.


