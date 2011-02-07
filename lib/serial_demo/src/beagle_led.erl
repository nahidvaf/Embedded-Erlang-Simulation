-module(beagle_led).
%-behaviour(gen_server).
%-define(SERVER, ?MODULE).

%-include("../include/io.hrl").
-define(ON, "1").
-define(OFF, "0").
-define(LED, "usr1").
-define(GPIO, "7").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, loop/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Args) ->
    {ok, spawn(?MODULE, init, [Args])}.
  %gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(ListenerPid) ->
    loop(ListenerPid).

loop(ListenerPid) ->
    receive
        {_Pid, {command, turn_on}} ->
            light(?ON);
        {_Pid, {command, turn_off}} ->
            light(?OFF);
        {Pid, {command, brightness}} ->
            Pid ! read_io()
    end,
    loop(ListenerPid).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
light(State) ->
    file:write_file("/sys/class/leds/beagleboard::" ++ ?LED ++ "/brightness", State).

read_io() ->
    case file:read_file("/sys/class/leds/beagleboard::" ++ ?LED ++ "/brightness") of
        {ok, <<?ON, _/binary>>} ->
            ?ON;
        {ok, <<?OFF, _/binary>>} ->
            ?OFF;
        _ ->
            {error, unknown_input}
    end.
