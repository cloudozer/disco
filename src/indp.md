# IvanOS Network Discovery Protocol
## Specification

Date: July 20, 2015

## Status
This is a draft document describing network discovery protocol which is a main 
mean of how each server in IvanOS network can find out the network topology.
The protocol is not complete. This is an initial phase of its development.


## History of changes

Date 			| Version	| Author		| What was changed		
July 20, 2015	| 0.1 		| Ian Tsybulkin	| Draft
				|			|				|

## Table of Contents


## Introduction


## Glossary

box - a server or a computer having one or more ports
wire - a an ethernet cable that connects two boxes to each other
IvanOS - a cloud operating system 



## The Network

Network represents an undirected graph; nodes of the graph represent servers while edges 
represent network connections between servers. We will call them boxes and wires.
Boxes may have more than one connection and can even be connected to itself.
A wire cannects to a box's port. Thus a connection looks like:

Box1 - port1 --- port2 - box2



## The Model

A process which is a part of a distibuted application may send a message to another process
that may run in a different container  and on a differen box. To deliver the message IvanOS
should find a path between the two given boxes. The proposed model assumes that 
network discovery app is a system application that runs on each box and constantly learns the network.
So, this is the only source of information about the network for any other processes. 
Network discovery (nd) explores the network talking to its neighbors - nd apps running on
neighbor boxes.

## nd packets

nd uses small ethernet packets to explore the network. It requires no other protocols or layers
to send or receive information to/from its neighbors. The format of the packet looks as it is 
shown below:

6 bytes			| 6 bytes			| 2 bytes	| 10 - 30 bytes
Destination_addr| Source_addr 		| Type1		| Data






