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
        %{"/", cowboy_static, {priv_file, discovery, "project1.html"}}
        {"/", cowboy_static, {file, "priv/project1.html"}},
%        {"/static/[...]", cowboy_static, {priv_dir, discovery, "static"}},
        {"/websocket", ws_handler, []}
      ]}
    ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
      [{env, [{dispatch, Dispatch}]}]),

  {ok, Pid} = gen_event:start_link({local, drc_event}),
  gen_event:add_handler(Pid, drc_event, []),

%%   gen_event:notify(drc_event, test),
%%   timer:apply_interval(2000, gen_event, notify, [drc_event, {notify, <<"hey">>}]),

    discovery_sup:start_link().

stop(_State) ->
    ok.
