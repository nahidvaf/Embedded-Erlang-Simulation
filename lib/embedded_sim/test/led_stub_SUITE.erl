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

-module(led_stub_SUITE).
-include_lib("common_test/include/ct.hrl").
-include("../include/serial.hrl").

%% Test server callbacks
-export([suite/0,
         init_per_suite/1, end_per_suite/1,
         init_per_testcase/2,end_per_testcase/2,
         all/0]).

%% Test cases
-export([turn_on_off/1]).

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
    {ok, Stub} = led_stub:start_server("",""),
    [{stub, Stub}|Config].

%%------------------------------------------------------------------------------
%% Teardowns
%%------------------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

end_per_testcase(_Case, Config) ->
    %Stub = proplists:get_value(serial_stub, Config),
    %Stub ! stop,
    ok.

%% Returns the list of groups and test cases to be tested
all() ->
    [turn_on_off].


%%------------------------------------------------------------------------------
%% Test cases
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% Description   : Tests turning on and off the led and verifies the status
%%------------------------------------------------------------------------------
turn_on_off() ->
    [].

turn_on_off(Config) ->
    Stub = proplists:get_value(stub, Config),

    % Check led status after started
    send_message(Stub, brightness),
    ok = receive_message(Stub, off),

    % Turn on and check status
    send_message(Stub, turn_on),
    send_message(Stub, brightness),
    ok = receive_message(Stub, on),

    % Turn off and check status
    send_message(Stub, turn_off),
    send_message(Stub, brightness),
    ok = receive_message(Stub, off),
    ok.

%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------
receive_message(Stub, Message) ->
    receive
        {Stub, Message} ->
            ok;
        Other ->
            {message_not_recognized, Other}
    after 1000 ->
            no_response_in_1000ms
    end.

send_message(Stub, Message) ->
    Stub ! {self(), {command, Message}}.
