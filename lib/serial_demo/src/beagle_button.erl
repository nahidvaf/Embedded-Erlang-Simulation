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
%% @doc Beagle button API for handling a port reading button device file
%% @end
%% ------------------------------------------------------------------------------
-module(beagle_button).
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

-export([init/1, loop/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------------------
%% @doc Starts a server handling a port reading from and event device file,
%% sending messages of when button is pushed and released
%% This is not a gen_server, because beagle board dit not have otp libs
%% @end
%% ------------------------------------------------------------------------------
start_link(Args) ->
    Pid = spawn_link(?MODULE, init, [Args]),
    {ok, Pid}.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(ListenerPid) ->
    {ok, Port} = button_stub:start_port({spawn, "/bin/cat </dev/input/event0"},
                            [use_stdio, stream, binary]),
    loop(Port, ListenerPid).

loop(Port, ListenerPid) ->
    receive
        {_Port, {data,<<_:64, 1:16/little,276:16/little,1:32/little,_/bitstring>>}} ->
            ListenerPid ! {self(), pushed};
        {_Port, {data,<<_:64, 1:16/little,276:16/little,0:32/little,_/bitstring>>}} ->
            ListenerPid ! {self(), released}
    end,
    loop(Port, ListenerPid).


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
