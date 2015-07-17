% simulator of link discovery service in ivanOS
%
% Cloudozer


-module(port).
-export([new/3,
		port/5,
		ping/3,
		pp/1,
		get_mac/0
		]).

-define(PING_INTERVAL,1000).
-define(PING_TIMEOUT,5000).
-define(PING_NBR, 10). 
-define(HIGH_THR,0.75). % percentage of pongs coming back during some interval
-define(LOW_THR, 0.25). % percentage of pongs coming back during some interval
 


new(Mac,Box,Links) ->
	Links ! {new_port,Mac,self()},  % register port in Links
	spawn(?MODULE,ping,[Mac,Box,Links]),
	receive
		{Box,Box_pid} -> port(disconnected,Mac,Box,Box_pid,Links)
	end.


% States:
%
% 1. {disconnected}
% 2. {unstable, {Neighbor_port, Neighbor_box}, Queue_of_pongs }
% 3. {stable, {Neighbor_port, Neighbor_box}, Queue_of_pongs }


port(State,Mac,Box,Box_pid,Links) ->
	receive
		{<<"FFFFFF">>,Source_port,<<"ND">>,{ping,Source_box} } ->
			Links ! {Source_port,Mac,<<"ND">>,{pong,Box} },
			port(State,Mac,Box,Box_pid,Links);

		{Mac,         Source_port,<<"ND">>,{pong,Source_box} } ->
			case State of
				disconnected ->
					port({unstable,{Source_port,Source_box},queue:from_list([os:timestamp()])},
						Mac,Box,Box_pid,Links);

				{Stable_Unstable, {Source_port, Source_box}, Pongs } ->
					T = os:timestamp(),
					Pong1 = remove_expired(T,queue:in(T,Pongs)),
					case {Stable_Unstable, is_stable(Stable_Unstable,Pong1)} of
						{Same,Same} -> 
							port({Stable_Unstable,{Source_port,Source_box},Pong1},Mac,Box,Box_pid,Links);

						{stable,unstable} ->
							port({unstable,{Source_port,Source_box},Pong1},Mac,Box,Box_pid,Links);

						{unstable,stable} ->
							Box_pid ! {new_connection, Mac, Source_port,Source_box },
							port({stable,{Source_port,Source_box},Pong1},Mac,Box,Box_pid,Links)
					end;


				{_,{Neighbor_port, Neighbor_box},_} ->  % a new port and/or box
					port({unstable,{Neighbor_port, Neighbor_box},queue:from_list([os:timestamp()])},
						Mac,Box,Box_pid,Links)

			end;


		{<<"FFFFFF">>,Source_port,<<"ND">>,{add_wire,_,_,_,_,_}=Msg } ->
			Box_pid ! {Mac,Source_port,Msg},
			port(State,Mac,Box,Box_pid,Links);

		{<<"FFFFFF">>,Source_port,<<"ND">>,{del_wire,_,_,_,_,_}=Msg } ->
			Box_pid ! {Mac,Source_port,Msg},
			port(State,Mac,Box,Box_pid,Links)
	after
		?PING_TIMEOUT ->
			Box_pid ! {lost_connection, Mac},
			port(disconnected,Mac,Box,Box_pid,Links)

	end.



ping(Mac,Box,Links) ->
	receive
	after 
		?PING_INTERVAL -> Links ! {<<"FFFFFF">>,Mac,<<"ND">>,{ping,Box} },
		ping(Mac,Box,Links)
	end.



remove_expired(T,Q) ->
	case timer:now_diff(T, queue:get(Q) ) > ?PING_NBR * ?PING_INTERVAL of
		true -> remove_expired(T,queue:drop(Q));
		false-> Q
	end.



is_stable(stable,Q) -> 
	case queue:len(Q) > ?PING_NBR * ?LOW_THR of
		true -> stable;
		false-> unstable
	end;
is_stable(unstable,Q) -> 
	case queue:len(Q) > ?PING_NBR * ?HIGH_THR of
		true -> stable;
		false-> unstable
	end.

	


pp(<<P1,P2,P3,P4,P5,P6>>) -> 
	byte_to_hex(P1)++":"++byte_to_hex(P2)++":"++
	byte_to_hex(P3)++":"++byte_to_hex(P4)++":"++
	byte_to_hex(P5)++":"++byte_to_hex(P6). 



byte_to_hex(B) ->
	S = integer_to_list(B,16),
	case length(S) of
		2 -> S;
		1 -> [$0|S]
	end.



get_mac() -> crypto:strong_rand_bytes(6).
	
