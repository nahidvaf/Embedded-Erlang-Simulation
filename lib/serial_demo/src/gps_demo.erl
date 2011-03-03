-module(gps_demo).
-compile(export_all).

start() ->
    ListenerPid     = spawn_link(?MODULE, init, []),
    register(?MODULE, ListenerPid),
    ListenerPid.

init() ->

    % Start gps receiver
    GPSPid       = serial:start([{speed, 4800}, {open, "/dev/ttyUSB0"}]),
    register(gps, GPSPid),

    % Start demo loop
    process(GPSPid).

process(GPSPid) ->
    receive Other -> io:format("~n~p~n", [Other]) end,
    process(GPSPid).

