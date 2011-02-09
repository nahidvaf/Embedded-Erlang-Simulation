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
%% @doc Beagle led API for reading and writing to a file representing led state
%% @end
%% ------------------------------------------------------------------------------

-module(beagle_led).
%-behaviour(gen_server).
%-define(SERVER, ?MODULE).

-include("../include/io.hrl").

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

%% ------------------------------------------------------------------------------
%% @doc Starts a server handling reading and writing to a file representing
%% the led state
%% @end
%% ------------------------------------------------------------------------------
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
