-module(serial_demo).

-compile(export_all).
-export([start/0, user_button/1]).

-define(ON, "1").
-define(OFF, "0").
-define(LED, "1").
-define(GPIO, "7").
-define(SIGNAL, "signal").

start() ->
    _SerialPid = serial:start([{speed, 115200}, {open, "/dev/ttyS2"}]),

    _ButtonPid = button_stub:start_server(beagle_button, []),

    _LedPid    = led_stub:start(beagle_led, []).

%%
% LED controller states
%%
init(SerialPid) ->
    light(?OFF),

    receive
        user_button ->
            signal(SerialPid),
            off(SerialPid);
        {data, <<?SIGNAL>>} ->
            on(SerialPid)
    end.


off(SerialPid) ->
    light(?OFF),

    receive
        {data, <<?SIGNAL>>} ->
            on(SerialPid)
    end.

on(SerialPid) ->
    light(?ON),

    receive
        user_button -> ok
    end,

    signal(SerialPid),
    off(SerialPid).

%%
% User button listener
%%
user_button(ListenerPid) ->
    file:write_file("/sys/class/gpio/export", ?GPIO),
    file:write_file("/sys/class/gpio/gpio"?GPIO, "in"),
    user_button_loop(ListenerPid).

user_button_loop(ListenerPid) ->
    case file:read_file("/sys/class/gpio/gpio"?GPIO) of
        {ok, <<?ON, _/binary>>} ->
            ListenerPid ! user_button;
        _ ->
            timer:sleep(100),
            user_button_loop(ListenerPid)
    end.

%%
% Internal mumbojumbo
%%
signal(SerialPid) ->
    SerialPid ! {send, <<?SIGNAL>>}.

light(State) ->
    file:write_file("/sys/class/leds/beagleboard::"?LED"/brightness", State).
