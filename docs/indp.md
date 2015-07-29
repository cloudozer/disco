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
| July 29, 2015 | 0.2 		| Ian Tsybulkin | A basic version is complete. There are a few open questions that need to be answered before moving on |


### Table of Contents

- Status
- Introduction
- Glossary
- The network
- The Model
- Network discovery packets
- Neighbors pinging
- State of connection
- Broadcasts
- Initiating broadcast
- Receiving and transmitting broadcast packets
- Open questions


### Introduction

IvanCloud is a cloud based on a network of commodity servers having one or more network ports
and connected with each other by Ethernet wires. There are no switches in the network. This is
a network where every server is a computing node and a switch at the same time.

It is assumed that every physical server is a node of the cloud that has all information about
the network. IvanCloud can consist of a single box. There is no need to manually join different
boxes in a network. Network discovery protocol described in this doc allows to find all nodes
automatically.


### Glossary

| Term  | Description |
|:------|:-------|
| box 		| a physical server or a computer having one or more ports |
| node 		| a computing and switching unit of IvanCloud. It is a box with a IvanOS installed and running on it |
| wire 		| a an Ethernet cable that connects two boxes to each other |
|IvanOS 	| a cloud operating system |



### The Network

Any network is represented by an undirected graph or simply a graph further in the doc;
nodes of the graph represent boxes while edges represent network connections or wires.
Boxes may have more than one connection and can even be connected to itself.
A wire connects to a box's port. Thus any connection looks like:

Box1 - Port1 ---//--- Port2 - Box2



### The Model

A process which is a part of a distributed application may send a message to another process
that may run in a different container  and on a different box. To deliver the message IvanOS
should find a path between the two given boxes. The proposed model assumes that 
network discovery app is a system application that runs on each box and constantly explores the network.
So, this is the only source of information about the network for any other processes.
Network discovery (ND) explores the network talking to its neighbors - ND apps running on
neighbor boxes.


### Network discovery packets

ND uses small Ethernet packets to explore the network. It requires no other protocols or layers
of abstraction to send or receive information to/from its neighbors. 
The format of the packet looks as it is shown below:

| 6 bytes			| 6 bytes			| 2 bytes	| ~10-30 bytes	 |
|:------------------|:------------------|:----------|:---------------|
| Destination_addr	| Source_addr 		| Type1		| Data 			 |



### Neighbors pinging

ND constantly pings all its neighbors sending ping packets to all its ports.
The Erlang format of ping packet is shown below. Later, when ND protocol 
will stabilize the Erlang format will be changed to bit format.

```Erlang
{<<"FFFFFF">>, Source_port, ND_type, {ping, Box, TS} }

```

where the first two fields are MAC addresses, the third one is a special type of all ND protocol packets,
Box is a source box name (ID), and TS is a time stamp.

If an ND app gets a ping packet from one of its neighbors, it responds sending pong packet back.
Pong packet contains the same TS as it was in the ping packet while substituting its own 
name in the My_box field. TS is a time stamp, so the box that sent ping can measure time of
round trip of a ping packet.


```Erlang
{Neighbor_port, My_port, ND_type, {pong, My_box, TS} } 

```


### State of connection

Pinging neighbors is a primary information source allowing to evaluate a connection health. 
If pongs go back on a regular base, the connection is healthy or stable.
A connection can be in one of four states shown in a table below:


| Connection state	| Description 				|
|:------------------|:--------------------------|
| disconnected  	| There were no pongs from port during some predefined period |
| unstable-disconnected | irregular pongs come from previously disconnected port |
| unstable-connected | The port was connected, however pongs are getting to come irregularly |
| stable 			| regular pongs are coming from port |


ND shares a change in its state with its neighbors only in two (depicted in red) out of ten possible cases:

- unstable-disconnected  ->  stable
- unstable-connected    ->  disconnected

A diagram showing all possible transitions between connection states depicted below:

![Connection state diagramm](https://github.com/tsybulkin/discovery/blob/master/docs/connection_states_diagram.jpg)



### Broadcasts

ND shares an update in its connection state with the rest of the network sending
broadcast packets to its neighbors. Neighbors read them, analyze, and either send farther or
drop stopping broadcasting wave.

Any box may initiate a broadcast wave if a certain condition triggers this.
A broadcast packet format is shown below:

```Erlang 
{ <<"FFFFFF">>, Source_port, ND_type, {BCM_type,TS, ...} }
```

where: 
BCM_type - one of possible broadcast message types;
TS - time stamp of the message

There two BCM_types so far used for information sharing:

	add_wire - a request to add a new connection between two boxes. 
	del_wire - a request to delete the given connection


### Initiating broadcast

A node initiates a broadcast in one of two cases described above in the connection state
diagram.
- when it explores a new stable connection
- when it looses a connection


#### New connection

If new stable connection is set, both nodes acts similar, so let us 
consider just one node describing what happens next. In the following 
diagram it is shown that box1 established a stable connection with box2:

![New connection diagram ](https://github.com/tsybulkin/discovery/blob/master/docs/new_connection_diadram.jpg)

Box1 will initiate two different types of update:
- it sends an info about its new connection to all its port except the established one.
- it sends an info about the whole network to newly established port

The first broadcast is simple:

```Erlang 
{ <<"FFFFFF">>, Source_port, ND_type, {add_wire,TS,Box1,Port1,Port2,Box2} }
```
Where Source_port is its own port to which it sends a broadcast.

The second broadcast is a group of broadcast packages, one packet per each wire of the network
Box1 belongs to.

Of course, the same wave of packets will come from its newly established neighbor about
its network.


#### Connection lost

If box loses one of its connections it send a simple broadcast to all its ports and pushes
TS to its broadcast archive.

```Erlang 
{ <<"FFFFFF">>, Source_port, ND_type, {del_wire,TS,Box1,Port1,Port2,Box2} }
```

Its neighbor sends a similar broadcast to its neighbors, so if boxes are connected
to each other through other boxes' connections, each box in the network will receive
two messages about the loss of connection. 

There is a chance that network will split into two parts due to some connection loss.
This is not a problem. We may check this running graph connectivity routine.

Note. Graph connectivity is a computationally intensive problem that may require a
significant amount of time to check connectivity of a large network. We should decide
later how often we need to run it. Perhaps, we may run it after each connection loss.
Alternatively, we may run it once per some period of time or when this info is needed.


### Receiving and transmitting broadcast packets

When broadcast packet initiated by other nodes comes to Port_in, it is 
checked if it had been once shown already. 

```Erlang 
{ <<"FFFFFF">>, Source_port, ND_type, {BCM_type,TS, ...} }
```

For checking that its time stamp TS is used. 
If broadcast archive contain that TS, the packet is dropped. Otherwise,
ND reads the info, update its graph database and sends the packet further to
all its ports except Port_in. TS is pushed to the broadcast archive.
This guarantees that the packet reaches all the nodes in the network, but eventually
be dropped by nodes which get it twice.


### Open questions


#### Pinging to test a connection
The current version of the protocol assumes that a connection can be tested
only by sending packets to a port and waiting for the response. Thus,
we do not separate two different events:
- a physical loss of connectivity between nodes and
- a temporarily connection loss due to congestions, traffic jams, or any other reasons.

To separate these two different types of problems with network connection we use two additional
unstable states described in the State of connection section of this doc. 
This helps us to separate short temporal problems from long-lasting 
consequences caused by plugging the wire off or turning the whole box off. 

If these two events can be explicitly and reliably separated on a physical level, the protocol can be simplified.


#### Bulky updates
The current implementation encountered an interesting behavior which hinders to determine
if a box was known (or connected to the network) before a new connection was established.
In other words it is not easy to determine whether a new connection leads to a merge of two
disconnected networks or just a new connection between two boxes belonging to the same network.

A simple solution which has been implemented considers any new connection as a merge that induces a larger wave of information update than it could be in the case when we add a new wire between two
existing boxes of our network.

This is a disadvantage of the current implementation, which can be fixed later. It will require
to add a handshake between wired boxes before they start sharing any information with each other and its neighbors.






