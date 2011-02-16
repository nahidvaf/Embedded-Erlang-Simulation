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

-module(recorder_SUITE).
-include_lib("common_test/include/ct.hrl").

%% Test server callbacks
-export([suite/0,
         init_per_suite/1, end_per_suite/1,
         init_per_testcase/2,end_per_testcase/2,
         all/0]).

%% Test cases
-export([]).

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
    Config.
%%------------------------------------------------------------------------------
%% Teardowns
%%------------------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

end_per_testcase(_Case, Config) ->
    ok.

%% Returns the list of groups and test cases to be tested
all() ->
    [].


%%------------------------------------------------------------------------------
%% Test cases
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% Test case name:
%% Description   :
%%------------------------------------------------------------------------------
record() ->
    [{require, recorder_test_message}].

record(Config) ->

    Pid = spawn(fun() ->
                        receive
                            {P, Any} ->
                                P ! Any
                        end
                end),
    dbg:p(Pid, [m]),
    Pid.

    Message = ct:get_config(recorder_test_message),

    % Test disconnected states
    SerialStub ! {self(), {command, [?SEND, Message]}},
    ok = receive_message(SerialStub, Message),
    % Send message twice so we test connected state as well
    SerialStub ! {self(), {command, [?SEND, Message]}},
    ok = receive_message(SerialStub, Message).


%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------
receive_message(SerialStub, Message) ->
    receive
        {SerialStub, {data, Message}} ->
            ok;
        Other ->
            {message_not_recognized, Other}
    after 1000 ->
            no_response_in_1000ms
    end.
