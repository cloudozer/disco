%
% simple digraph library 
% 
%

% digraph is a tuple containing two lists: 
% list of graph nodes and list of graph edges
%

-module(digr).
-export([new/0,size/1,
		has_node/2,has_edge/2,
		add_node/2,add_node/3,
		add_node_attr/3, node_attrs/2,get_node_attr/3,
		add_edge/2,
		nodes/1,edges/1,neighbors/2,incidents/2,
		export_to_dot/2]).


% creates new digraph
new() ->
	{[],[]}.

size({Nodes,Edges}) -> length(Nodes)+length(Edges).


% returns true/false if Graph has/has_not Node  
has_node(Node,{Nodes,_}) ->
	lists:member(Node,[ Nd || {Nd,_} <- Nodes]).
	

% returns true/false if Graph has/has_not Edge
has_edge(Edge,{_,Edges}) ->
	lists:member(Edge,Edges).


% returns new graph with Node added
add_node(Node_id,{Nodes,Edges}=Gr) ->
	case has_node(Node_id,Gr) of
		false-> {[{Node_id,[]}|Nodes],Edges};
		true -> error("Node already exists in the graph")
	end.
add_node(Node_id,Attrs,{Nodes,Edges}=Gr) ->
	case has_node(Node_id,Gr) of
		false-> {[{Node_id,Attrs}|Nodes],Edges};
		true -> error("Node already exists in the graph")
	end.



% Adds node's attribute. Return new graph
add_node_attr(Node,{_,Edges}=Gr,Attr) ->
	case has_node(Node,Gr) of
		true -> 
			{{Node,Attrs},Rest} = extract(Node,Gr),
			{[ {Node,[Attr|Attrs]} | Rest ],Edges};
		false-> error("There is no Node:"++Node)
	end.



% returns list of node attributes
node_attrs(Node,Gr) ->
	{{Node,Attrs},_}=extract(Node,Gr),
	Attrs.

	
% returns value for Attribute or none if it does not exist
get_node_attr(Attr,Node,Gr) ->
	{{Node,Attrs},_}=extract(Node,Gr),
	D = dict:from_list(Attrs),
	case dict:is_key(Attr,D) of
		true -> dict:fetch(Attr,D);
		false-> none
	end.



% returns new graph
add_edge({Node1,Node2}=Edge,{Nodes,Edges}=Gr) ->
	case has_node(Node1,Gr) andalso has_node(Node2,Gr) andalso 
		not (has_edge(Edge,Gr) orelse has_edge({Node2,Node1},Gr)) of
		true -> {Nodes,[Edge|Edges]};
		false-> error("Either edge already exists or one node does not exist in the graph")
	end.


nodes({Nodes,_}) ->
	[ Nd ||{Nd,_} <- Nodes].


edges({_,Edges}) ->
	Edges.


% returns all child nodes of Node
neighbors(Node,Gr) ->
	[ {N2,Attrs} || {N1,N2} <- edges(Gr), {N3,Attrs} <- digr:nodes(Gr), N1==Node,N2==N3].


% returns all parent nodes of Node
incidents(Node,Gr) ->
	[ N1 || {N1,N2} <- edges(Gr), N2 == Node].


export_to_dot(Gr,Gname) ->
	File = Gname++".dot",
	{ok,Out} = file:open(File, [write]),
	io:format(Out, "digraph ~s {~n",[Gname]),
	
	export_to_dot(edges,digr:edges(Gr),Out).

	
	 
export_to_dot(_,[],Out) ->
	io:format(Out,"}",[]),
	file:close(Out);
export_to_dot(_,[{Nd1,Nd2}|Edges],Out) ->
	io:format(Out,"\"~p\" -> \"~p\" ;~n",[Nd1,Nd2]),
	export_to_dot(edges,Edges,Out).

	



% returns tuple: {{node,its_attrs},Rest_of_nodes}
extract(Node,{Nodes,_}) ->
	extract([],Node,Nodes).

extract(Acc,Node,[{Node,Attrs}|Nodes]) ->
	{{Node,Attrs},Acc++Nodes};
extract(Acc,Node,[{Nd,Attrs}|Nodes]) ->
	extract([{Nd,Attrs}|Acc],Node,Nodes).






