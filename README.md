# Embedded-Erlang-Simulation
A library, simulators and demo applications for simulating hardware on erlang

## Introduction
The project is based on a thesis conducted at Chalmers Univeristy of Technology under supervison of Erlang Solutions. The goal of the project is to develop simulators for device drivers and to create and describe a good design/test/work flow for developing on Erlang Embedded.

Testing code on hardware frequently is inconvenient, takes time and only possible if you have access to the hardware. By simulating simple devices like serial, buttons, leds, GPS, etc we aim to prove that we can create a better development environment. A work flow will be created to describe how to work with Erlang Embedded with simulators and the current development environment will be improved by adding convenient scripts to, for example, deploy code to hardware. The idea is that you should be able to switch between hardware and simulated mode without having to change the code, just by changing an environmental variable and providing any neccesary config files.

Our thesis is related to Erlang Embedded which is based on two other thesis projects conducted at Erlang Solutions by four students from Uppsala University, see [http://embedded-erlang.org/] [2]. Erlang embedded provides a linux distro and an erlang release which is tailored for embedded hardware, and the Beagle Board in specific. We continue on their work and use their release and the same hardware.

## Serial Demo
We are using rebar to build and generate nodes.

To build the Embedded-Erlang-Simulation run ./rebar compile from the project root folder.

We have setup two nodes under nodes called sim_node and sim_node2, run ./rebar generate to generate the nodes.

### Run on Desktop using Simulators

Compile and generate the nodes if you havn't already.

To compile and generate run from the project root

`./rebar compile`

`./rebar generate`

Open two terminals and set os env to "sim" on both

`export EMBEDDED_ENV=sim`

Now start sim_node in one of the terminals and sim_node2 in the other

`nodes/sim_node/sim_node/bin/sim_node console`

`nodes/sim_node2/sim_node2/bin/sim_node2 console`

In both nodes start the serial demo

`serial_demo:start().`

To simulate a button held down during 3 seconds

`button_stub ! {self(), {command, {push, 3000}}}.`

Now the simulator monitor should print on the screen the states of the button in the node you pushed it, and the states of the led in the other node.

### Run on Hardware

To run serial demo on two beagle boards with erlang embedded setup all you need to do is put the code on the memory cards, you can use scp over usb network. Remember to compile before uploading the code.

`./rebar compile`

Connect two beagle boards over serial and make sure the rx tx are crossed over the serial, you have to use a crossed serial cable. Then do the following on both the boards.

Login to the beagleboard over serial or usb network. See [erlang embedded][2] and [beagle board][3] how to setup it up.

From the top folder of Embedded-Erlang-Simulation

`cd lib/erlang-serial`

Fire up an erlang shell with pz

`/opt/local/erlang/erts-'version'/bin/erl -pz ebin ../embedded-sim/ebin ../serial-demo/ebin`

Run start the serial demo

`serial_demo:start().`

If you push the user1 button on one of the boards the usr1 led should light up on the other board, when you release the button it is should turn off again.

## Examples
See `lib/serial_demo/serial_demo.erl`

## Contribute
If you find any issues with Erlang-Embedded-Simulation or have any comments please [create an issue] [1]

## Licenses
Erlang-Embedded-Simulation exists under different open-source licenses.

### Erlang-Embedded-Simulation
Is licensed under:
Apache License
Version 2.0, January 2004
http://www.apache.org/licenses/

Includes serial_demo and embedded_sim

### erlang-serial
Copyright (c) 1996, 1999 Johan Bevemyr
Copyright (c) 2007, 2009 Tony Garnock-Jones

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


[1]: https://github.com/EmbeddedErlang/Embedded-Erlang-Simulation/issues "Erlang-Embedded-Simulation issues"

[2]: http://embedded-erlang.org/ "Erlang-Embedded webpage"

[3]: http://beagleboard.org/ "Beagle Board"