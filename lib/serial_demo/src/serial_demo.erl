%% ------------------------------------------------------------------------------
%% @author Rickard Olsson <rickard@ooodev.com>
%% @author Reza Javaheri <reza@ooodev.com>
%% @doc Demo application for showing how Erlang Embedded Simulation can be used
%% @end
%% ------------------------------------------------------------------------------

-module(serial_demo).

-compile(export_all).
-export([start/0, init/1]).

-include("../include/io.hrl").

start() ->
    _ListenerPid     = spawn_link(?MODULE, init, [self()]).

init(MonitorPid) ->
    % Start ui monitor
    _StubMonitor     = stub_monitor:start_link(),
    % Start device servers
    SerialPid       = serial:start([{speed, 115200}, {open, "/dev/ttyS2"}]),
    register(serial, SerialPid),
    {ok, LedPid}    = led_stub:start_server(beagle_led, [self()]),
    {ok, ButtonPid} = button_stub:start_server(beagle_button, [self()]),
    % Start demo loop
    process(SerialPid, LedPid, ButtonPid, MonitorPid).

process(SerialPid, LedPid, ButtonPid, MonitorPid) ->
    receive
        {data, <<?OFF>>} ->
            MonitorPid ! data_off_recieved,
            light(LedPid, ?OFF);
        {data, <<?ON>>} ->
            MonitorPid ! data_on_recieved,
            light(LedPid, ?ON);
        {ButtonPid, pushed} ->
            MonitorPid ! user_pushed_recieved,
            signal(SerialPid, <<?ON>>);
        {ButtonPid, released} ->
            MonitorPid ! user_released_recieved,
            signal(SerialPid, <<?OFF>>)
    end,
    process(SerialPid, LedPid, ButtonPid, MonitorPid).

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
