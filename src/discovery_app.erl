-module(discovery_app).

-behaviour(application).

-export([start/0]).
%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
  ok = application:start(crypto),
  ok = application:start(ranch),
  ok = application:start(cowlib),
  ok = application:start(cowboy),
  ok = application:start(discovery).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
      {'_', [
        {"/:box", cowboy_static, {file, "priv/monitor.html"}},
        {"/websocket/:box", ws_handler, []}
      ]}
    ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
      [{env, [{dispatch, Dispatch}]}]),

    discovery_sup:start_link().

stop(_State) ->
    ok.
