%% ------------------------------------------------------------------------------
%% Copyright 2011 Rickard Olsson, Reza Javaheri
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%% ------------------------------------------------------------------------------

%% ------------------------------------------------------------------------------
%% @author Rickard Olsson <rickard@ooodev.com>
%% @author Reza Javaheri <reza@ooodev.com>
%% @doc Demo application for showing how Erlang Embedded Simulation can be used
%% @end
%% ------------------------------------------------------------------------------

-module(serial_demo).

-compile(export_all).
-export([start/0, init/0, send_keep_alive/1]).

-include("../include/io.hrl").

start() ->
    ListenerPid     = spawn_link(?MODULE, init, []),
    register(?MODULE, ListenerPid),
    ListenerPid.

init() ->
    % Start ui monitor
    _StubMonitor     = stub_monitor:start_link(),

    % Start device servers
    SerialPid       = serial:start([{speed, 115200}, {open, "/dev/ttyS2"}]),
    register(serial, SerialPid),

    % Start keep alive process for serial
    spawn_link(?MODULE, send_keep_alive, [SerialPid]),

    {ok, LedPid}    = led_stub:start_server(beagle_led, [self()]),

    {ok, ButtonPid} = beagle_button:start_link(self()),
    register(beagle_button, ButtonPid),

    % Start demo loop
    process(SerialPid, LedPid, ButtonPid).

process(SerialPid, LedPid, ButtonPid) ->
    receive
        {data, <<?KEEP_ALIVE>>} ->
            keep_alive_received;
        {data, <<?OFF>>} ->
            light(LedPid, ?OFF);
        {data, <<?ON>>} ->
            light(LedPid, ?ON);
        {ButtonPid, pushed} ->
            signal(SerialPid, <<?ON>>);
        {ButtonPid, released} ->
            signal(SerialPid, <<?OFF>>)
    end,
    process(SerialPid, LedPid, ButtonPid).

send_keep_alive(SerialPid) ->
    signal(SerialPid, <<?KEEP_ALIVE>>),
    timer:sleep(2000),
    send_keep_alive(SerialPid).

%--------------------------------------------------------------------------------
% Internal Functions
%--------------------------------------------------------------------------------
signal(SerialPid, Data) ->
    SerialPid ! {send, Data}.

light(LedPid, State) ->
    LedPid ! translate(State).

translate(?ON) ->
    {self(), {command, turn_on}};
translate(?OFF) ->
    {self(), {command, turn_off}}.
