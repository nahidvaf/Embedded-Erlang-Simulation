-module(serial_demo).

-compile(export_all).
-export([start/0, user_button_loop/2, init/1]).

-define(ON, "1").
-define(OFF, "0").
-define(LED, "1").
-define(GPIO, "7").

start() ->

    ListenerPid     = spawn_link(?MODULE, init, [self()]),
    {ok, ButtonPid} = button_stub:start_server(beagle_button, []),
    spawn_link(?MODULE, user_button_loop, [ButtonPid, ListenerPid]).

%%
% LED controller states
%%
init(Monitor_Pid) ->
    {ok, LedPid}    = led_stub:start_server(beagle_led, []),
    SerialPid       = serial:start([{speed, 115200}, {open, "/dev/ttyS2"}]),
    process(SerialPid, LedPid, Monitor_Pid).

process(SerialPid, LedPid, Monitor_Pid) ->
    receive
        {data, <<?OFF>>} ->
            Monitor_Pid ! data_off_recieved,
            light(LedPid, ?OFF);
        {data, <<?ON>>} ->
            Monitor_Pid ! data_on_recieved,
            light(LedPid, ?ON);
        user_button_pushed ->
            Monitor_Pid ! user_pushed_recieved,
            signal(SerialPid, <<?ON>>);
        user_button_released ->
            Monitor_Pid ! user_released_recieved,
            signal(SerialPid, <<?OFF>>)
    end,
    process(SerialPid, LedPid, Monitor_Pid).

%%
% User button listener
%%
%% user_button(ListenerPid) ->
%%     file:write_file("/sys/class/gpio/export", ?GPIO),
%%     file:write_file("/sys/class/gpio/gpio"?GPIO, "in"),
%%     user_button_loop(ListenerPid).

user_button_loop(ButtonPid, ListenerPid) ->
    ButtonPid ! {self(), {command, state}},
    receive
        {ButtonPid, pushed} ->
            ListenerPid ! user_button_pushed;
        {ButtonPid, released} ->
            ListenerPid ! user_button_released
        after 1000 ->
            exit(button_simulator_not_responding)
    end,
    timer:sleep(10),
    user_button_loop(ButtonPid, ListenerPid).

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
