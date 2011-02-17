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
-export([record/1]).

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
    rec:stop(),
    ok.

%% Returns the list of groups and test cases to be tested
all() ->
    [record].


%%------------------------------------------------------------------------------
%% Test cases
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% Test case name:
%% Description   :
%%------------------------------------------------------------------------------
record() ->
    [{require, recorder_test_message},
     {require, recorder_test_file}].

record(Config) ->

    % init stuff
    Message = {self(), ct:get_config(recorder_test_message)},
    File = ct:get_config(recorder_test_file),
    file:delete(File),
    rec:start(File),

    TestFun = fun() ->
	    receive
		{P, Any} ->
		    ok
		    %P ! Any
	    end,
        receive
            _ ->
                ok
        end
    end,

    % Test tracing one process
    Pid1 = spawn(TestFun),
    rec:add_process(Pid1),
    Pid1 ! Message,

    timer:sleep(100),
    {ok, Terms1} = file:consult(File),
    [{trace, {delay, _}, {pid, Binary_Pid1}, {type, 'receive'},
      {msg, Binary_Message1}}] = Terms1,
    Pid1 = binary_to_term(Binary_Pid1),
    Message = binary_to_term(Binary_Message1),

    % Test tracing another process
    Pid2 = spawn(TestFun),
    rec:add_process(Pid2),
    Pid2 ! Message,

    timer:sleep(100),
    {ok, Terms2} = file:consult(File),
    [_,{trace, {delay, _}, {pid, Binary_Pid2}, {type, 'receive'},
        {msg, Binary_Message2}}] = Terms2,
    Pid2 = binary_to_term(Binary_Pid2),
    Message = binary_to_term(Binary_Message2),


    Pid1 ! Message,

    timer:sleep(100),
    {ok, Terms3} = file:consult(File),
    [_,_,{trace, {delay, _}, {pid, Binary_Pid3}, {type, 'receive'},
        {msg, Binary_Message3}}] = Terms3,
    Pid1 = binary_to_term(Binary_Pid3),
    Message = binary_to_term(Binary_Message3),

    ok.

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
