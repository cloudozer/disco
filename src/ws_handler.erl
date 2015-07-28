-module(ws_handler).
-export([init/2]).
-export([websocket_info/3]).
-export([websocket_handle/3]).

-record(state, {box}).

init(Req, _Opts) ->
    Box = cowboy_req:binding(box,Req),
    %io:format("ws_handler init, box: ~p~n", [Box]),
    {cowboy_websocket, Req, #state{box = erlang:binary_to_atom(Box, latin1)}}.



% messages from Box process
websocket_info({add_box,Box}, Req, State) ->
	Json = jsx:encode([{type,add_box},{name,Box}]),
	%io:format("JSON: ~s~n",[Json]),
	{reply, {text, Json}, Req, State};

% messages from Box process
websocket_info({add_wire,Box1,Box2}, Req, State) ->
	Json = jsx:encode([{type,add_wire},{b1,Box1},{b2,Box2}]),
	%io:format("JSON: ~s~n",[Json]),
	{reply, {text, Json}, Req, State};

% messages from Box process
websocket_info({del_wire,Box1,Box2}, Req, State) ->
	Json = jsx:encode([{type,del_wire},{b1,Box1},{b2,Box2}]),
	%io:format("JSON: ~s~n",[Json]),
	{reply, {text, Json}, Req, State};


websocket_info({entire_net,Boxes,Wires}, Req, State) ->
	%io:format("WS: got message from Box~n"),
	Json = jsx:encode([{type,entire_net},{boxes,Boxes},{wires,Wires}]),
	%io:format("Json: ~p~n",[Json]),
	{reply, {text, Json}, Req, State}.



% messages from web_monitor
websocket_handle({text,<<"get_entire_net">>}, Req, State = #state{box = Box}) ->
     Box ! {get_entire_net, self()},
     {ok, Req, State}.
     %{reply, {text, <<"{\"type\": \"entire_network\"}">>}, Req, State}.
% websocket_handle(Frame, Req, State) ->
%     io:format("ws_handler websocket_handle ~p~n", [Frame]),
%     {ok, Req, State}.
