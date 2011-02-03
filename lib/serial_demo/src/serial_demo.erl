-module(serial_demo).

-compile(export_all).
-export([start/0, init/1]).

-define(ON, "1").
-define(OFF, "0").
-define(LED, "1").
-define(GPIO, "7").

start() ->
    _ListenerPid     = spawn_link(?MODULE, init, [self()]).
%%
% LED controller states
%%
init(MonitorPid) ->
    % Start ui monitor
    _StubMonitor     = stub_monitor:start_link(),
    % Start device servers
    SerialPid       = serial:start([{speed, 115200}, {open, "/dev/ttyS2"}]),
    {ok, LedPid}    = led_stub:start_server(beagle_led, []),
    {ok, ButtonPid} = button_stub:start_server(beagle_button, []),
    % Start demo loop
    process(SerialPid, LedPid, ButtonPid, MonitorPid).

process(SerialPid, LedPid, ButtonPid, MonitorPid) ->
    receive
        {data, <<?OFF>>} ->
            %MonitorPid ! data_off_recieved,
            light(LedPid, ?OFF);
        {data, <<?ON>>} ->
            %MonitorPid ! data_on_recieved,
            light(LedPid, ?ON);
        {ButtonPid, pushed} ->
            %MonitorPid ! user_pushed_recieved,
            signal(SerialPid, <<?ON>>);
        {ButtonPid, released} ->
            %MonitorPid ! user_released_recieved,
            signal(SerialPid, <<?OFF>>)
    end,
    process(SerialPid, LedPid, ButtonPid, MonitorPid).

%%
% User button listener
%%
%% user_button(ListenerPid) ->
%%     file:write_file("/sys/class/gpio/export", ?GPIO),
%%     file:write_file("/sys/class/gpio/gpio"?GPIO, "in"),
%%     user_button_loop(ListenerPid).

%%
% Internal mumbojumbo
%%
signal(SerialPid, Data) ->
    SerialPid ! {send, Data}.

light(LedPid, State) ->
    LedPid ! translate(State).

translate(?ON) ->
    {self(), {command, turn_on}};
translate(?OFF) ->
    {self(), {command, turn_off}}.
