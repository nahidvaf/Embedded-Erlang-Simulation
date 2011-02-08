%% -----------------------------------------------------------------
%% @author Rickard Olsson <rickard@oodev.com>
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
