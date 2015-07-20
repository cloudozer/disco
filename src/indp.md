# IvanOS Network Discovery Protocol
## Specification

Date: July 20, 2015

### Status
This is a draft document describing network discovery protocol which is a main 
mean of how each server in IvanOS network can find out the network topology.
The protocol is not complete. This is an initial phase of its development.


### History of changes

| Date 			| Version	| Author		| What was changed	|
|:--------------|----------:|:--------------|:------------------|		
| July 20, 2015	| 0.1 		| Ian Tsybulkin	| Draft 			|

### Table of Contents


### Introduction


### Glossary

box - a server or a computer having one or more ports
wire - a an ethernet cable that connects two boxes to each other
IvanOS - a cloud operating system 



### The Network

Network represents an undirected graph; nodes of the graph represent servers while edges 
represent network connections between servers. We will call them boxes and wires.
Boxes may have more than one connection and can even be connected to itself.
A wire cannects to a box's port. Thus a connection looks like:

Box1 - port1 --- port2 - box2



### The Model

A process which is a part of a distibuted application may send a message to another process
that may run in a different container  and on a differen box. To deliver the message IvanOS
should find a path between the two given boxes. The proposed model assumes that 
network discovery app is a system application that runs on each box and constantly learns the network.
So, this is the only source of information about the network for any other processes. 
Network discovery (nd) explores the network talking to its neighbors - nd apps running on
neighbor boxes.

### nd packets

nd uses small ethernet packets to explore the network. It requires no other protocols or layers
of abstraction to send or receive information to/from its neighbors. 
The format of the packet looks as shown below:

| 6 bytes			| 6 bytes			| 2 bytes	| 10 - 30 bytes	 |
|:------------------|:------------------|:----------|:---------------|
| Destination_addr	| Source_addr 		| Type1		| Data 			 |


### Neighbors pinging

Each box constantly pings all its neighbors sending ping packets to all its ports.
The Erlang format of ping packet is shown below.

```Erlang
{<<"FFFFFF">>, Source_port, ND_type, {ping, Box} }

```


If a box got a ping packet from one of its neighbors, it responds sending pong packet:

```Erlang
{Neighbor_port, My_port, ND_type, {pong, My_box} } 

```

### State of connection

Any box evaluates the state of each of its connections. A connection can be in one of three
states:

disconnected - no pongs from port
unstable connection - low number of pongs from port
stable connection - sufficient number of pongs from port


### Broadcasts

Any box may initiate a broadcast update if a certain condition trigers this.
A broad cast packet format is shown below:

```Erlang 
{ <<"FFFFFF">>, Source_port, ND_type, {BCM_type,TS, ...} }

```

where: 
BCM_type - one of possible broadcast message types;
TS - time stamp of the message

There two BCM_types:
add_wire - a request to add a new connection between two boxes. 
del_wire - a request to delete the given connection









