-module(ws_handler).
-export([init/2]).
-export([websocket_info/3]).
-export([websocket_handle/3]).

-record(state, {}).

init(Req, _Opts) ->
    Box = cowboy_req:binding(box,Req),
    io:format("ws_handler init, box: ~p~n", [Box]),
    erlang:send_after(2000, self(), add),
    {cowboy_websocket, Req, #state{}}.

websocket_info(add, Req, State) ->
    %% Send a text to a websocket
    {reply, {text, <<"[]">>}, Req, State}.

websocket_handle(Frame, Req, State) ->
    io:format("ws_handler websocket_handle ~p~n", [Frame]),
    {ok, Req, State}.
