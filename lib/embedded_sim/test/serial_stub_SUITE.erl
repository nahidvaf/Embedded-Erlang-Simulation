%% -----------------------------------------------------------------
%% @author Rickard Olsson <rickard@ooodev.com>
%% @author Reza Javaheri <reza@ooodev.com>
%% ------------------------------------------------------------------

-module(serial_stub_SUITE).
-include_lib("common_test/include/ct.hrl").
-include("../include/serial.hrl").

%% Test server callbacks
-export([suite/0,
         init_per_suite/1, end_per_suite/1,
         init_per_testcase/2,end_per_testcase/2,
         all/0]).

%% Test cases
-export([connect/1, disconnect/1, open/1, close/1, speed/1, parity_odd/1,
         parity_even/1, break/1, driver_send/1, remote_send/1,
         not_alive_node/1]).

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

init_per_testcase(not_alive_node, Config) ->
    application:set_env(embedded_sim, serial_client_node, fake_node),
    {ok, SerialStub} = serial_stub:start_port("",""),
    [{serial_stub,SerialStub}|Config];
init_per_testcase(_Case, Config) ->
    application:set_env(embedded_sim, serial_client_node, node()),
    {ok, SerialStub} = serial_stub:start_port("",""),
    [{serial_stub,SerialStub}|Config].

%%------------------------------------------------------------------------------
%% Teardowns
%%------------------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

end_per_testcase(_Case, Config) ->
    %SerialStub = proplists:get_value(serial_stub, Config),
    %SerialStub ! stop,
    application:unset_env(embedded_sim, client_node),
    ok.

%% Returns the list of groups and test cases to be tested
all() ->
    [connect, disconnect, open, close, speed, parity_odd, parity_even, break,
     driver_send, remote_send, not_alive_node].


%%------------------------------------------------------------------------------
%% Test cases
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% Test case name: driver_send
%% Description   : In this test we mock the driver and send a message to
%% the other end and we expect the simulator to echo it back.
%%------------------------------------------------------------------------------
driver_send() ->
    [{require, serial_test_message}].

driver_send(Config) ->
    Message = ct:get_config(serial_test_message),
    SerialStub = proplists:get_value(serial_stub, Config),
    % Test disconnected states
    SerialStub ! {self(), {command, [?SEND, Message]}},
    ok = receive_message(SerialStub, Message),
    % Send message twice so we test connected state as well
    SerialStub ! {self(), {command, [?SEND, Message]}},
    ok = receive_message(SerialStub, Message).

%%------------------------------------------------------------------------------
%% Test case name: remote_send
%% Description   : In this test we mock the other party and send a
%% message the simulator and we expect a message to be delivered
%% to the driver.
%%------------------------------------------------------------------------------
remote_send() ->
    [{require, serial_test_message}].

remote_send(Config) ->
    Message = ct:get_config(serial_test_message),
    SerialStub = proplists:get_value(serial_stub, Config),
    gen_fsm:send_event(SerialStub,
                       {{serial_stub, node()}, {command, Message}}),
    ok = receive_message(SerialStub, Message).

%%------------------------------------------------------------------------------
%% Description :
%%------------------------------------------------------------------------------
connect() ->
    [{require, serial_test_message}].
connect(Config) ->
    Message = ct:get_config(serial_test_message),
    SerialStub = proplists:get_value(serial_stub, Config),
    SerialStub ! {self(), {command, [?CONNECT]}},
    no_response_in_1000ms = receive_message(SerialStub, Message),
    ok.

%%------------------------------------------------------------------------------
%% Description :
%%------------------------------------------------------------------------------
disconnect() ->
    [{require, serial_test_message}].
disconnect(Config) ->
    Message = ct:get_config(serial_test_message),
    SerialStub = proplists:get_value(serial_stub, Config),
    SerialStub ! {self(), {command, [?DISCONNECT]}},
    no_response_in_1000ms = receive_message(SerialStub, Message),
    ok.

%%------------------------------------------------------------------------------
%% Description :
%%------------------------------------------------------------------------------
open() ->
    [{require, serial_test_message}].
open(Config) ->
    Message = ct:get_config(serial_test_message),
    SerialStub = proplists:get_value(serial_stub, Config),
    SerialStub ! {self(), {command, [?OPEN, Message]}},
    no_response_in_1000ms = receive_message(SerialStub, Message),
    ok.

%%------------------------------------------------------------------------------
%% Description :
%%------------------------------------------------------------------------------
close() ->
    [{require, serial_test_message}].
close(Config) ->
    Message = ct:get_config(serial_test_message),
    SerialStub = proplists:get_value(serial_stub, Config),
    SerialStub ! {self(), {command, [?CLOSE]}},
    no_response_in_1000ms = receive_message(SerialStub, Message),
    ok.

%%------------------------------------------------------------------------------
%% Description :
%%------------------------------------------------------------------------------
speed() ->
    [{require, serial_test_message}].
speed(Config) ->
    Message = ct:get_config(serial_test_message),
    SerialStub = proplists:get_value(serial_stub, Config),
    SerialStub ! {self(), {command, [?SPEED, "we"," ", "we", 0]}},
    no_response_in_1000ms = receive_message(SerialStub, Message),
    ok.

%%------------------------------------------------------------------------------
%% Description :
%%------------------------------------------------------------------------------
parity_odd() ->
    [].
parity_odd(Config) ->
    Message = ct:get_config(serial_test_message),
    SerialStub = proplists:get_value(serial_stub, Config),
    SerialStub ! {self(), {command, [?PARITY_ODD]}},
    no_response_in_1000ms = receive_message(SerialStub, Message),
    ok.

%%------------------------------------------------------------------------------
%% Description :
%%------------------------------------------------------------------------------
parity_even() ->
    [].
parity_even(Config) ->
    Message = ct:get_config(serial_test_message),
    SerialStub = proplists:get_value(serial_stub, Config),
    SerialStub ! {self(), {command, [?PARITY_EVEN]}},
    no_response_in_1000ms = receive_message(SerialStub, Message),
    ok.

%%------------------------------------------------------------------------------
%% Description :
%%------------------------------------------------------------------------------
break() ->
    [{require, serial_test_message}].
break(Config) ->
    Message = ct:get_config(serial_test_message),
    SerialStub = proplists:get_value(serial_stub, Config),
    SerialStub ! {self(), {command, [?BREAK]}},
    no_response_in_1000ms = receive_message(SerialStub, Message),
    ok.


%%------------------------------------------------------------------------------
%% Description : Try to send serial message to a non existing process
%%------------------------------------------------------------------------------
not_alive_node() ->
    [{require, serial_test_message}].
not_alive_node(Config) ->
    Message = ct:get_config(serial_test_message),
    SerialStub = proplists:get_value(serial_stub, Config),
    SerialStub ! {self(), {command, [?SEND, Message]}},
    no_response_in_1000ms = receive_message(SerialStub, Message),
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
