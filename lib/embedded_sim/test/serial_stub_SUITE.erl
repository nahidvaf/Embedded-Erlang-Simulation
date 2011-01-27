-module(serial_stub_SUITE).
-include_lib("common_test/include/ct.hrl").

%% Test server callbacks
-export([suite/0,
         init_per_suite/1, end_per_suite/1,
         init_per_testcase/2,end_per_testcase/2,
         all/0]).

%% Test cases
-export([serial_message/1]).

%%------------------------------------------------------------------------------
%% Common test callbacks
%%------------------------------------------------------------------------------

suite() ->
    [{timetrap, {minutes, 1}}].

init_per_suite(Config) ->
    true = os:putenv("EMBEDDED_ENV", "sim"),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(serial_message, Config) ->
    application:set_env(embedded_sim, serial_client_node, node()),
    Config;
init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(serial_message, _Config) ->
    application:unset_env(embedded_sim, serial_client_node),
    ok;
end_per_testcase(_Case, _Config) ->
    ok.

%% Returns the list of groups and test cases to be tested
all() ->
    [serial_message].


%%------------------------------------------------------------------------------
%% Test cases
%%------------------------------------------------------------------------------

serial_message() ->
    [{require, serial_test_message}].

serial_message(_Config) ->
    Message = ct:get_config(serial_test_message),
    {ok, Port} = serial_stub:start_port("",""),
    Port ! {self(), {command, Message}},
    ok = receive
        {Port, {data, Message}} ->
            ok;
        Other ->
            disallowed_message_received
        after 1000 ->
            no_response_in_1000ms
    end.
