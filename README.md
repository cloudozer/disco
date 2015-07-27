# Network Discovery Protocol

Date: July 2, 2015

## Overview

Network discovery application is a simulator which consisted of three parts that are closely 
connected with each other:

 - a local network emulator
 - a network explorer
 - a monitor for better visualization of the network discovery process

This main goal of this application is to facilitate the process of development and verification
of network discovery protocol which should become a basic feature of IvanOS.

## Dependecies

- cowboy, a web server for monitoring network discovery
- jsx, JSON converter library needed for intagration of Erlang apps and JS app (Monitor)
- Erlang

## How to install application and run it

1. Clone the repo 
	
	```
	https://github.com/tsybulkin/discovery.git

	cd discovery
	```

	Then compile the application and run it:

	```
	./rebar co
	erl -pa ebin deps/*/ebin -s discovery_app
	``` 

## Network simulation

Before creating your network you need to create a wire hub, a special process for 
manipulating with wires, connecting or disconnecting boxes and so on.

	```
	W = wire:new().
	```

W is an Erlang process that will help you control your network.

Then you may add new boxes. Each box must have a unique id. I recommend assign a simple ids like
box1, box2, and so on for simplicity. You also need to specify the number of ports
the box has and an ID of the network hub:

	```
	box:new(box1,2,W).
	box:new(box2,3,W).
	```

In the example above we added two new boxes with ids: box1 and box2, specifying that box1 has two ports
while box2 has 3 ports. The created boxes are disconnected, so they do not see each other.

You may open a monitor to watch the status of any box. Open a browser and enter:

	```
	http://localhost:8080/box1
	```

A little blue ball should appear in the browser showing that box1 knows only about itself. There are no
neighbors in its network.

To look at the box2's network information open browser at http://localhost:8080/box2


## Wire hub API

There are a few commands you may use to manipulate with you network:

	```
	box:new(Box_id, Port_nbr, W)
	```



