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
		is_known/2,
		add_neighbor/5
		%export_to_dot/2
		]).


% creates new network
new(Box) -> { dict:store(Box,[],dict:new()), dict:new()}.  % {Network,Wires,Ports}

% returns size of known network
size({Network,_}) -> dict:size(Network).


% returns true/false if Graph has/has_not Node  
has_box(Box,{Network,_}) -> dict:is_key(Box,Network).
	

% returns the list of known boxes
boxes({Network,_}) -> dict:fetch_keys(Network).


is_known(Box,{Network,_}) -> dict:is_key(Box,Network).



add_neighbor(Box1,Port1,Port2,Box2,{Network,Links}) ->
	Network1 = dict:append(Box2,Box1,dict:append(Box1,Box2,Network)),
	Links1 = dict:append({Box2,Box1},{Port2,Port1},dict:append({Box1,Box2},{Port1,Port2},Links)),
	{Network1,Links1}.


