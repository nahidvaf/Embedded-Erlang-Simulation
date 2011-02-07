-module(beagle_button).
%-behaviour(gen_server).
%-define(SERVER, ?MODULE).

%-include("../include/io.hrl").
-define(ON, "1").
-define(OFF, "0").
-define(LED, "1").
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

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(ListenerPid) ->
    _ButtonPort = open_port({spawn, "/bin/cat </dev/input/event0"}, [use_stdio, stream, binary]),
    loop(ListenerPid).

loop(ListenerPid) ->
    receive
        {_, {data,<<_:64, 1:16/little,276:16/little,1:32/little,_/bitstring>>}} ->
            ListenerPid ! {self(), pushed};
        {_, {data,<<_:64, 1:16/little,276:16/little,0:32/little,_/bitstring>>}} ->
            ListenerPid ! {self(), released}
    end,
    loop(ListenerPid).


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
