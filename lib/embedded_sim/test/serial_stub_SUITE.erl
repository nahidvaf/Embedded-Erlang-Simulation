-module(serial_stub_SUITE).
-include_lib("common_test/include/ct.hrl").

%% Test server callbacks
-export([suite/0,
         init_per_suite/1, end_per_suite/1,
         init_per_testcase/2,end_per_testcase/2,
         all/0]).

%% Test cases
-export([serial_driver_msg/1, serial_remote_msg/1, serial_not_alive_node/1]).

%%------------------------------------------------------------------------------
%% Common test callbacks
%%------------------------------------------------------------------------------

suite() ->
    [{timetrap, {minutes, 1}}].

%%------------------------------------------------------------------------------
%% Setups
%%------------------------------------------------------------------------------
init_per_suite(Config) ->
    true = os:putenv("EMBEDDED_ENV", "sim"),
    Config.

init_per_testcase(serial_not_alive_node, Config) ->
    application:set_env(embedded_sim, serial_client_node, fake_node),
    Config;
init_per_testcase(_Case, Config) ->
     application:set_env(embedded_sim, serial_client_node, node()),
    Config.

%%------------------------------------------------------------------------------
%% Teardowns
%%------------------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

end_per_testcase(_Case, _Config) ->
    application:unset_env(embedded_sim, serial_client_node),
    ok.

%% Returns the list of groups and test cases to be tested
all() ->
    [serial_driver_msg, serial_remote_msg, serial_not_alive_node].


%%------------------------------------------------------------------------------
%% Test cases
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% Test case name: serial_driver_msg
%% Description   : In this test we mock the driver and send a message to
%% the other end and we expect the simulator to echo it back.
%%------------------------------------------------------------------------------
serial_driver_msg() ->
    [{require, serial_test_message}].

serial_driver_msg(_Config) ->
    Message = ct:get_config(serial_test_message),
    {ok, Port} = serial_stub:start_port("",""),
    % Test disconnected states
    Port ! {self(), {command, Message}},
    ok = receive_message(Port, Message),
    % Send message twice so we test connected state as well
    Port ! {self(), {command, Message}},
    ok = receive_message(Port, Message).

%%------------------------------------------------------------------------------
%% Test case name: serial_remote_msg
%% Description   : In this test we mock the other party and send a
%% message the simulator and we expect a message to be delivered
%% to the driver.
%%------------------------------------------------------------------------------
serial_remote_msg() ->
    [{require, serial_test_message}].

serial_remote_msg(_Config) ->
    Message = ct:get_config(serial_test_message),
    {ok, SerialStub} = serial_stub:start_port("",""),
    gen_fsm:send_event(SerialStub, {{serial_stub, node()}, {command, Message}}),
    ok = receive_message(SerialStub, Message).

%%------------------------------------------------------------------------------
%% Description : Try to send serial message to a non existing process
%%------------------------------------------------------------------------------
serial_not_alive_node() ->
    [{require, serial_test_message}].
serial_not_alive_node(_Config) ->
    Message = ct:get_config(serial_test_message),
    {ok, Port} = serial_stub:start_port("",""),
    Port ! {self(), {command, Message}},
    no_response_in_1000ms = receive_message(Port, Message),
    ok.



%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------
receive_message(Port, Message) ->
    receive
        {Port, {data, Message}} ->
            ok;
        Other ->
            message_not_recognized
    after 1000 ->
            no_response_in_1000ms
    end.
