% simulator of link discovery service in ivanOS
%
% Cloudozer


-module(port).
-export([new/3,
		ping/2,
		pp/1,
		get_mac/0
		]).

-define(PING_INTERVAL,100).
-define(PING_TIMEOUT,5000).
-define(PING_NBR, 5). 
-define(HIGH_THR,0.7). % percentage of pongs coming back during some interval
-define(LOW_THR, 0.3). % percentage of pongs coming back during some interval
 


new(Mac,Box,Links) ->
	Links ! {new_port,Mac,self(),Box},  % register port in Links
	spawn(?MODULE,ping,[Mac,Links]),
	port(disconnected,Mac,Box,Links).


% States:
%
% 1. {disconnected}
% 2. {unstable_disconnected, {Neighbor_port, Neighbor_box}, Queue_of_pongs }
% 3. {unstable_connected, {Neighbor_port, Neighbor_box}, Queue_of_pongs }
% 4. {stable, {Neighbor_port, Neighbor_box}, Queue_of_pongs }


port(State,Mac,Box,Links) ->
	receive

	%% PING

		{<<"FFFFFF">>,Source_port,<<"ND">>,{ping,TS} } ->
			Pkt = {Source_port,Mac,<<"ND">>,{pong,TS,Box} },
			Links ! {pkt,Mac,Pkt},
			port(State,Mac,Box,Links);

	%% PONG

		{Mac,         Source_port,<<"ND">>,{pong,TS,Source_box} } ->
			case State of
				disconnected ->
					port({unstable_disconnected,{Source_port,Source_box},queue:from_list([TS])},
						Mac,Box,Links);

				{Stable_Unstable, {Source_port, Source_box}, Pongs } ->
					T = os:timestamp(),
					Pongs1 = remove_expired(T,queue:in(T,Pongs)),
					case {Stable_Unstable, is_stable(Stable_Unstable,Pongs1)} of
						{Same,Same} -> 
							port({Stable_Unstable,{Source_port,Source_box},Pongs1},Mac,Box,Links);

						{stable,unstable} ->
							io:format("Port ~s - STABLE connection~n",[pp(Mac)]),
							port({unstable_connected,{Source_port,Source_box},Pongs1},Mac,Box,Links);

						{unstable_disconnected,stable} ->
							io:format("Port ~s - STABLE connection~n",[pp(Mac)]),
							Box ! {new_connection, Mac, Source_port,Source_box },
							port({stable,{Source_port,Source_box},Pongs1},Mac,Box,Links);

						{unstable_disconnected,unstable} ->
							port({unstable_disconnected,{Source_port,Source_box},Pongs1},Mac,Box,Links);

						{unstable_connected,stable} ->
							port({stable,{Source_port,Source_box},Pongs1},Mac,Box,Links)
					end;

				{_,{Neighbor_port, Neighbor_box},_} ->  % a new port and/or box
					Box ! {lost_connection, Mac},
					port({unstable_disconnected,{Neighbor_port, Neighbor_box},queue:from_list([TS])},
						Mac,Box,Links)

			end;

	%% UPDATE BROADCAST

		{<<"FFFFFF">>,_,<<"ND">>,_Msg }=Pkt ->
			Box ! {pkt,Mac,Pkt},
			port(State,Mac,Box,Links)


	after
		?PING_TIMEOUT ->
			case State of
				disconnected -> port(disconnected,Mac,Box,Links);
				{unstable_disconnected,_,_} -> port(disconnected,Mac,Box,Links);
				_ ->
					Box ! {lost_connection, Mac},
					port(disconnected,Mac,Box,Links)
			end
	end.




ping(Mac,Links) ->
	receive
	after 
		?PING_INTERVAL -> 
			Pkt = {<<"FFFFFF">>,Mac,<<"ND">>,{ping,os:timestamp()} },
			Links ! {pkt,Mac,Pkt},
			ping(Mac,Links)
	end.




remove_expired(T,Q) ->
	case timer:now_diff(T, queue:get(Q) ) > ?PING_NBR * ?PING_INTERVAL*1000 of
		true -> remove_expired(T,queue:drop(Q));
		false-> Q
	end.



is_stable(stable,Q) -> 
	case queue:len(Q) > ?PING_NBR * ?LOW_THR of
		true -> stable;
		false-> unstable
	end;
is_stable(_,Q) -> 
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
	
