# Network Discovery Protocol

Date: July 2, 2015

## Overview

Network discovery application is a simulator which consisted of three parts that are closely 
connected with each other:

 - a local network emulator
 - a network explorer
 - a monitor for better visualization of the network discovery process

The main goal of this application is to facilitate the process of development and verification
of network discovery protocol which should become one of basic capabilities of IvanOS.

## Dependencies

- cowboy, a web server for monitoring network discovery
- jsx, JSON converter library needed for integration of Erlang apps and JS app (Monitor)
- Erlang

## How to install application and run it

First, clone the repo: 
	
```
git clone https://github.com/cloudozer/disco.git
cd disco
```

Then, compile the application and run it:

```
./rebar get-deps
./rebar co
erl -pa ebin deps/*/ebin -s discovery_app
``` 

## Network simulation

Before creating any network you need to spawn a wire hub, a special process for 
manipulating with wires, connecting or disconnecting boxes and so on.

```erlang
1> W = wire:new().
2> 
```

W is the id of an Erlang process that will help you to control the emulated network.

Then, you may add new boxes. Each box must have a unique id. I recommend choosing a simple ids like
box1, box2, and so on for simplicity reason. You also need to specify the number of ports
the box has and an ID of the network hub:

``` erlang
2> box:new(box1,2,W).
<0.156.0>
3> box:new(box2,3,W).
<0.162.0>
4> 
```

In the example above we added two new boxes with ids: box1 and box2, specifying that box1 has two ports
while box2 has 3 ports. The created boxes are disconnected, so they do not see each other.

You may open a monitor to watch the status of any box. Open a browser and enter:


	http://localhost:8080/box1


A little blue ball should appear in the browser showing that box1 knows only about itself. There are no
neighbors in its network.

To look at the box2 network information open browser at http://localhost:8080/box2


## Wire hub API

There are a few commands you may use to manipulate with you network:

|Command | Description |
|:---|:---|
| box:new(Box_id,Port_nbr,W)| Box_id - a unique id; You may use atoms instead of numbers or strings. Port_nbr = integer > 0, specifying number of ports. W - a wire hub, the process for network manipulation |
| wire:free_ports(W)		| Returns a list of boxes' ports that are not connected at the moment |
| wire:wires(W) 			| Shows all connection in the network 							|
| W ! {add_wire,Port1,Port2}| Connects two given ports with a wire. If one or both ports are already connected, it ignores the command showing a warning 	|
| W ! {del_wire,Port1,Port2}| Disconnect the given ports. If the ports are not connected with each other, it ignores the command showing a warning |
| wire:ask(Box_ID,get_net) 	| Returns the entire network discovered by Box_ID |

## Generating random network

You may generate a random network containing a given number of boxes. Each box in the network will have
the following IDs: box1, box2, ... , boxN.
It will be randomly wired assuming that each box has 4 ports, so any wire will connect two different boxes.
It is possible to have more than one wire between two particular boxes.

```Erlang
1> nd:run(40).
2>
```

The function above will generate a random network containing 40 boxes. The process of network generation
can be observed if you attach a monitor to any network box, say box1.

The function returns a undirected graph in a form of adjacency list. When the network is built you may
find what a particular box discovered about the network. For that run:

```Erlang
3> wire:ask(box1,get_net).
4>
```

This function returns a discoverd network in the form of adjacency list too, but with port information.
To compare two graphs, an input one against a discovered one, run the following function:

```Erlang
5> nd:verify(G1,G2).
6> 
```

Of course, before you need to assign to G1 and to G2 the values returning by nd:run(N) and wire:ask(box1,get_net) functions accordingly.




