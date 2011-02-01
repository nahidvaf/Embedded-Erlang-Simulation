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
    {ok, Stub} = button_stub:start_server("",""),
    [{stub, Stub}|Config].

%%------------------------------------------------------------------------------
%% Teardowns
%%------------------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

end_per_testcase(_Case, Config) ->
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

    % Check button status after started
    send_message(Stub, state),
    ok = receive_message(Stub, released),

    % Pushes button and verifies it got pushed
    send_message(Stub, push),
    send_message(Stub, state),
    ok = receive_message(Stub, pushed),

    % Waits and verifies button got release
    timer:sleep(500),
    send_message(Stub, state),
    ok = receive_message(Stub, released).

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

