# Embedded-Erlang-Simulation
A library, simulators and demo applications for simulating hardware on erlang

## Introduction
The project is based on a thesis conducted at Chalmers Univeristy of Technology under ssupervison of Erlang Solutions. The goal of the project is to develop simulators for device drivers and to create and describe a good design/test/work flow for developing on Erlang Embedded.

Testing code on hardware frequently is inconvenient, takes time and only possible if you have access to the hardware. By simulating simple devices like serial, buttons, leds, GPS, etc we aim to prove that we can create a better development environment. A work flow will be created to describe how to work with Erlang Embedded with simulators and the current development environment will be improved by adding convenient scripts to, for example, deploy code to hardware. The idea is that you should be able to switch between hardware and simulated mode without having to change the code, just by changing an environmental variable and providing any neccesary config files.

Our thesis is related to Erlang Embedded which is based on two other thesis projects conducted at Erlang Solutions by four students from Uppsala University, see http://embedded-erlang.org/. Erlang embedded provides a linux distro and an erlang release which is tailored for embedded hardware, and the Beagle Board in specific. We continue on their work and use their release and the same hardware.

## Examples
see `lib/serial_demo/serial_demo.erl`

## Contribute
If you find any issues with Erlang-Embedded-Simulation or have any comments please [create and issue] [1]

[1]: https://github.com/EmbeddedErlang/Embedded-Erlang-Simulation/issues "Erlang-Embedded-Simulation issues"
