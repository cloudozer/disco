# IvanOS Network Discovery Protocol
## Specification

Date: July 20, 2015

### Status
A version 1.0 of the protocol got a significant change. The main point is in removing
bulky updates when two networks are merged. In fact, there is one method to discover 
neighbors - pinging, and one method to share information about discovered neighbors -
broadcasting. An updates about losing a connection are removed. Each node may discover
this itself by not having the corresponding updates. It means that all information about
the network is expiring. New updates that a node gets refresh it. No updates about information
make it expired.
The protocol may need some small addition. It can be implemented as a stand alone application
working on top of LINCX.


### History of changes

| Date 			| Version	| Author		| What was changed	|
|:--------------|----------:|:--------------|:------------------|		
| July 20, 2015	| 0.1 		| Ian Tsybulkin	| Draft 			|
| July 29, 2015 | 0.2 		| Ian Tsybulkin | A basic version is complete. There are a few open questions that need to be answered before moving on |
| August 3, 2015| 1.0 		| Ian Tsybulkin | A protocol was simplified; Bulky updates removed |


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
automatically and keep information about the network updated.


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

A wire has exactly two ends connecting two boxes in a network.


### The Model

A process which is a part of a distributed application may send a message to another process
that may run in a different container and on a different box. To deliver the message IvanOS
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

ND constantly pings all its neighbors sending ping-packets to all its ports.
The Erlang format of ping packet is shown below. Later, when ND protocol 
will stabilize the Erlang format will be changed to a bit format.

```Erlang
{<<"FFFFFF">>, Source_port, ND_type, {ping, TS} }

```

where the first two fields are MAC addresses, the third one is a special type of all ND protocol packets, and TS is a time stamp.

If an ND app gets a ping packet from one of its neighbors, it responds sending pong-packet back.
pong-packet contains the same TS as it was in the ping-packet while adding its own 
name in the My_box field. TS is a time stamp, so the box that sent ping can measure time of
round trip of a ping packet.


```Erlang
{Neighbor_port, My_port, ND_type, {pong, TS, My_box} } 

```


### State of connection

Pinging neighbors is a primary information source allowing to evaluate a connection health. 
If pongs go back on a regular basis, the connection is healthy or stable.
A connection can be in one of four states shown in a table below:


| Connection state	| Description 				|
|:------------------|:--------------------------|
| disconnected  	| There were no pongs from port during some predefined period |
| unstable-disconnected | irregular pongs come from previously disconnected port |
| unstable-connected | The port was connected, however pongs are getting to come irregularly |
| stable 			| regular pongs are coming from port |


ND shares a change in its state from unstable-disconnected to a stable state at once the change in the state occur. 

If there were no events in state change as described above, during some period of time (say 1 sec)
ND broadcasts the state of a connection unless it is in disconnected state. The message looks like the following:

```Erlang
{<<“FFFFFF”>>, SourcePort, <<“”ND””>>,  stable, TimeStamp, SourceBox, NeibPort, NeibBox}
or
{<<“FFFFFF”>>, SourcePort, <<“”ND””>>,  unstable, TimeStamp, SourceBox, NeibPort, NeibBox}
```

![Connection state diagramm](https://github.com/tsybulkin/discovery/blob/master/docs/connection_states_diagram.jpg)



### Broadcasts

ND shares an update in its connection state with the rest of the network sending
broadcast packets to its neighbors. Neighbors read them, analyze, and either send farther or
drop stopping broadcasting wave.

Any box may initiate a broadcast wave if a certain condition triggers this.
A broadcast packet format is shown below:

```Erlang 
{<<“FFFFFF”>>, SourcePort, <<“”ND””>>,  stable, TimeStamp, SourceBox, NeibPort, NeibBox}
```

The first thing each box does when it receives a broadcast message is checking if it has already
got this message. For that purpose ND keeps an archive of all broadcast messages it received
during last 10 seconds or so. If a message is older than that, ND drops it as expired and irrelevant.

Otherwise, it forwards it to the rest of its ports, adds its time stamp to its archive, 
and then updates its database.


#### Initiating broadcast

A node initiates a broadcast in one of two cases described above in the connection state
diagram.
- when it explores a new stable connection
- when some time period passes.


### Archive

ND maintains its archive keeping time stamps of all of its broadcasts during last 10 seconds or so.
Archive looks like a queue. An old messages are cleaned up from one end and new come from the other end of the queue.


### Discovered graph
It is a matter of each box to keep its discovered graph updated. It may regularly clean up the graph
removing the nodes and edges which have no updates during some period of time.







