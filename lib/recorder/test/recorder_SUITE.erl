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
-export([record/1, replay/1]).

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

init_per_testcase(Case, Config) when (Case == replay) or (Case == record ) ->
    register(test_pid, self()),
    Config.
%%------------------------------------------------------------------------------
%% Teardowns
%%------------------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

end_per_testcase(replay, _Config) ->
    unregister(test_pid),
    rec:stop(),
    ok;
end_per_testcase(record, _Config) ->
    unregister(test_pid),
    ok.

%% Returns the list of groups and test cases to be tested
all() ->
    [record, replay].


%%------------------------------------------------------------------------------
%% Test cases
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% Test case name:
%% Description   :
%%------------------------------------------------------------------------------

replay() ->
    [{require, log_file}].

replay(_Config) ->
    % init
    LogFile = ct:get_config(log_file),
    replayer:start(LogFile),

    receive message_one -> ok end,
    receive message_two -> ok end,
    receive message_three -> ok end,

    ok.

record() ->
    [{require, recorder_test_message},
     {require, recorder_test_file}].

record(_Config) ->

    % init stuff
    Message = ct:get_config(recorder_test_message),
    File = ct:get_config(recorder_test_file),
    file:delete(File),
    rec:start(File, [send, 'receive']),

    TestFun = fun() ->
	    receive _ -> test_pid ! ok end,
        receive _ -> test_pid ! ok end,
        receive _ -> test_pid ! ok end
    end,

    % Test tracing one process
    Pid1 = spawn(TestFun),
    register(test_fun1, Pid1),
    rec:add_process(Pid1),
    Pid1 ! Message,

    timer:sleep(100),
    {ok, Terms1} = file:consult(File),
    [{trace, {delay, _}, {pid, _RegProc1}, {type, 'receive'},{msg, Message}},
     {trace, {delay, _}, {pid, _}, {type, 'send'}, {msg, ok}, {to, test_pid}}]
     = Terms1,

    % Test tracing another process
    Pid2 = spawn(TestFun),
    register(test_fun2, Pid2),
    rec:add_process(Pid2),
    Pid2 ! Message,

    timer:sleep(100),
    {ok, Terms2} = file:consult(File),
    [_, _,{trace, {delay, _}, {pid, _RegProc2}, {type, 'receive'},{msg, Message}},
    {trace, {delay, _}, {pid, _}, {type, 'send'}, {msg, ok}, {to, test_pid}}]
     = Terms2,

    Pid1 ! Message,

    timer:sleep(100),
    {ok, Terms3} = file:consult(File),
    [_,_,_,_,{trace, {delay, _}, {pid, _RegProc}, {type, 'receive'},{msg, Message}},
    {trace, {delay, _}, {pid, _}, {type, 'send'}, {msg, ok}, {to, test_pid}}]
     = Terms3,

    ok.

%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------
