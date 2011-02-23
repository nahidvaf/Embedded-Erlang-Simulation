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

%% -----------------------------------------------------------------
%% @author Rickard Olsson <rickard@ooodev.com>
%% @author Reza Javaheri <reza@ooodev.com>
%% ------------------------------------------------------------------

-module(button_stub_SUITE).
-include_lib("common_test/include/ct.hrl").
-include("../include/serial.hrl").

%% Test server callbacks
-export([suite/0,
         init_per_suite/1, end_per_suite/1,
         init_per_testcase/2,end_per_testcase/2,
         all/0]).

%% Test cases
-export([push/1]).

%%------------------------------------------------------------------------------
%% Common test callbacks
%%------------------------------------------------------------------------------

suite() ->
    [{timetrap, {minutes, 1}}].

%%------------------------------------------------------------------------------
%% Setups
%%------------------------------------------------------------------------------
init_per_suite(Config) ->
    os:putenv("EMBEDDED_ENV", "sim"),
    Config.

init_per_testcase(_Case, Config) ->
    {ok, Stub} = button_stub:start_port("",""),
    [{stub, Stub}|Config].

%%------------------------------------------------------------------------------
%% Teardowns
%%------------------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

end_per_testcase(_Case, _Config) ->
    %Stub = proplists:get_value(stub, Config),
    %Stub ! stop,
    ok.

%% Returns the list of groups and test cases to be tested
all() ->
    [push].

%%------------------------------------------------------------------------------
%% Test cases
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% Description   : Pushes the button and verifies the state
%%------------------------------------------------------------------------------
push() ->
    [].

push(Config) ->
    Stub = proplists:get_value(stub, Config),

    % Pushes button and verifies it got pushed
    send_message(Stub, {push, 200}),
    ok = receive_message(Stub, to_binary(pushed), 1000),
    ok = receive_message(Stub, to_binary(released), 1000).

%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------
receive_message(Stub, Message, TimeOut) ->
    receive
        {Stub, {data, Message}} ->
            ok;
        Other ->
            {message_not_recognized, Other}
    after TimeOut ->
            timeout_no_response
    end.

send_message(Stub, Message) ->
    Stub ! {self(), {command, Message}}.

to_binary(released) ->
     <<0:64, 1:16/little, 276:16/little, 0:32/little, 0:1>>;
to_binary(pushed) ->
     <<0:64, 1:16/little, 276:16/little, 1:32/little, 0:1>>.
