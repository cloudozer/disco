% simulator of link discovery service in ivanOS
%
%

-module(disc).

-export([]).



% runs simulation for the given sequence of events and returns the log file
% of network states
%
% events look like: {Tick,connect,box1,box2} or {Tick,disconnect,box1,box2}
%
simulate(Box_nbr, Event_seq) -> 
	Network = lists:foldl(fun(J,Gph)-> digr:add_node(J,Gph) end, digr:new(),lists:seq(1,Box_nbr)),
	simulate(Box_nbr,lists:sort(Event_seq),Network,0,erlang:make_tuple(Box_nbr,{queue:new(),[],digr:new()}), []).

simulate(Box_nbr,[{Tick,connect,Box1,Box2}|Event_seq],Network,Tick, State, Log) ->





% Messages:
%
% ping - 
%

