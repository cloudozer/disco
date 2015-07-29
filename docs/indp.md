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
- Open questions


### Introduction

IvanCloud is a cloud based on a network of commodity servers having one or more network ports
and connected with each other by ethernet wires. There are no switches in the network. This is
a network where every server is a computing node and a swith at the same time.

It is assumed that every physical server is a node of the cloud that has all information about
the network. IvanCloud can consist of a single box. There is no need to manually join different
boxes in a network. Network discovery protocol described in this doc allows to find all nodes
automatically.


### Glossary

| Term  | Description |
|:------|:-------|
|box 		| a physical server or a computer having one or more ports |
|node 		| a computing and switching unit of IvanCloud. It is a box with a IvanOS installed and running on it |
|wire 		| a an ethernet cable that connects two boxes to each other |
|IvanOS 	| a cloud operating system |



### The Network

Any network is represented by an undirected graph or simply a graph further in the doc;
nodes of the graph represent boxes while edges represent network connections or wires.
Boxes may have more than one connection and can even be connected to itself.
A wire cannects to a box's port. Thus any connection looks like:

Box1 - Port1 ---//--- Port2 - Box2



### The Model

A process which is a part of a distibuted application may send a message to another process
that may run in a different container  and on a differen box. To deliver the message IvanOS
should find a path between the two given boxes. The proposed model assumes that 
network discovery app is a system application that runs on each box and constantly explores the network.
So, this is the only source of information about the network for any other processes.
Network discovery (ND) explores the network talking to its neighbors - ND apps running on
neighbor boxes.


### Network discovery packets

ND uses small ethernet packets to explore the network. It requires no other protocols or layers
of abstraction to send or receive information to/from its neighbors. 
The format of the packet looks as it is shown below:

| 6 bytes			| 6 bytes			| 2 bytes	| ~10-30 bytes	 |
|:------------------|:------------------|:----------|:---------------|
| Destination_addr	| Source_addr 		| Type1		| Data 			 |



### Neighbors pinging

ND constantly pings all its neighbors sending ping packets to all its ports.
The Erlang format of ping packet is shown below. Later, when ND protocol 
will stabilize the erlang format will be changed to bit format.

```Erlang
{<<"FFFFFF">>, Source_port, ND_type, {ping, Box, TS} }

```

where the first two fields are MAC addresses, the third one is a special type of all ND protocol packets,
Box is a source box name (ID), and TS is a time stamp.

If an ND app gets a ping packet from one of its neighbors, it responds sending pong packet back.
Pong message contains the same TS as it was in the ping packet while substituting its own 
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
| unstable-diconnected | irregular pongs come from the port |
| unstable-connected | The port was connected, however pongs got to come irregularly |
| stable 			| regular pongs are coming from port |


ND shares a change in its state with its neighbors only in two out of ten possible cases:

- unstable-siconnected  ->  stable
- unstanle-connected    ->  disconnected

A diagram showing all possible transitions between connection states depicted below:

![Connection state diagramm](https://github.com/tsybulkin/discovery/blob/master/docs/connection_states_diagram/connection_states_diagram.001.jpg)



### Broadcasts

ND shares an update in its connection state with the rest of the network sending
broadcast packets to its neighbors. Neighbors read them, analyze, and either send farther or
drop stopping broadcasting wave.

Any box may initiate a broadcast wave if a certain condition trigers this.
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


### Open questions

1. The current version of the protocol assumes that a connection can be tested
only by sending packets to a port and waiting for the response. Thus,
we do not separate two different events:
- disconneting nodes physically pluging off the wire from the port or swiching off
 a computer and
- a temporarily connection lost due to congestion, traffic jam, or any other reasons.

To separate these two different types of problems with network connection we use two additional
unstable states. This helps us to separate short temporal problems from long-lasting 
consequences caused by pluging wire off or turning whole box off. 

If these two events can be read figured out using other means, the protocol can be simplified a bit.


2. The current implementation encountered an interesting behaivor which hinders to determine
if a box was known (or connected to the network) before a new connection was established.
In other words it is not easy to determine whether a new connection leads to a merge of two
disconnected networks or just a new connection between two boxes belonging to the same network.

A simple solution that was implemented considers any new connection as a merge that induces a larger 
wave of information update than it could be in the case when we add a new wire between two
existing boxes in our network.

This is a disadvantage of a current implementation, which can be fixed later. It will require
to add a handshake between wired boxes before they start sharing any information with each other and its neighbors.






