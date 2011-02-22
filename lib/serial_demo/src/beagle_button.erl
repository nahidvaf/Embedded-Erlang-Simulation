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

-export([init/1, loop/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------------------
%% @doc Starts a server handling a port reading from and event device file,
%% sending messages of when button is pushed and released
%% @end
%% ------------------------------------------------------------------------------
start_link(Args) ->
    Pid = spawn(?MODULE, init, [Args]),
    register(?MODULE, Pid),
    {ok, Pid}.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(ListenerPid) ->
    _ButtonPort = open_port({spawn, "/bin/cat </dev/input/event0"},
                            [use_stdio, stream, binary]),
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
