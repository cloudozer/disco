# Monitoring box's network info API

July 22, 2015


## Overview

This doc describes how a browser can be used to monitor box's network
information.


## Connections
The emulator represents a bunch of erlang processes that emulates the work
of boxes and ports. One can monitor a box's info about the network using
a special application that runs in the browser. It communicates with an erlang process
through Cowboy web server. The connection looks like:

Browser  <-->  WS  <--> BOX

## Start of application

First, you need to run Network discovery app using Erlang shell.
Then you may create a several boxes - which are Erlang processes. Then you may
decide to monitor one box or another. For that, you need to open in a broweser

	```javascript
	http://localhost:8080/box_name
	```

it will automatically conects to the process 'box_name' which is a registered
name of Erlang process representing a certain box you like to monitor.
You may open more than one monitor if you need to monitor several boxes simultaneously.



## Messages between WS and BOX
Both WS and BOX are Erlang processes, so they use Erlang notation for messages they exchange
between each other.


### WS sends to BOX

	```erlang
	BOX ! {monitor, self(), get_entire_network} 
	```
WS sends this kind of message in the beginning and when page is refreshed.

### BOX sends to WS

Box send two types of messages to WS. One is the message containing the entire network
info:

	```erlang
	WS ! {entire_net, Nodes, Links}
	```

Nodes is a list of Boxes that the network consists of. Links is a list of wires connecting boxes
between each other. The connections are tuples {Box1, Box2}.

The message that Box may send to WS looks like one of the messages below:

	```erlang
	WS ! {add_box, New_box}
	WS ! {del_box, Box}
	WS ! {add_wire, Box1, Box2}
	WS ! {del_wire, Box1, Box2}
	```

	

