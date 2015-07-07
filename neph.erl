%
% simple graph library 
% 
%

% digraph is a tuple containing two lists: 
% list of graph nodes and list of graph edges
%

-module(neph).
-export([new/2,size/1,
		has_box/2
		%add_node/2,add_node/3,
		%add_node_attr/3, node_attrs/2,get_node_attr/3,
		%add_edge/2,
		%nodes/1,edges/1,neighbors/2,incidents/2,
		%export_to_dot/2
		]).


% creates new digraph
new(Box,Ports) -> { dict:store(Box,[],dict:new()), dict:new(), Ports }.  % {Network,Wires,Ports}

% returns size of known network
size({Network,_,_}) -> dict:size(Network).


% returns true/false if Graph has/has_not Node  
has_box(Box,{Network,_,_}) -> dict:is_key(Box,Network).
	





