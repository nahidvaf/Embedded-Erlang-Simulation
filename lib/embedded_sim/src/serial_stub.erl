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

%% ------------------------------------------------------------------
%% @author Rickard Olsson <rickard@ooodev.com>
%% @author Reza Javaheri <reza@ooodev.com>
%% @doc Simulates serial port driver
%% @end
%% ------------------------------------------------------------------

-module(serial_stub).

-author("Rickard Olsson").
-author("Reza Javaheri").

-behaviour(gen_fsm).

-define(SERVER, ?MODULE).
-include("../include/serial.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_port/2]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4,
        disconnected/2, connected/2, alive_check/1]).


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------------------
%% @doc Starts a serial stub or a port depending on OS env var EMBEDDED_SIM
%% @end
%% ------------------------------------------------------------------------------
start_port(PortName, PortSettings) ->
    stub:start_port(?MODULE, PortName, PortSettings).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    case application:get_env(embedded_sim, serial_client_node) of
        {ok, SerialClientNode} ->
            {ok, disconnected, [{serial_client_node, SerialClientNode}|Args]};
        undefined ->
            {stop, app_env_var_serial_client_node_unset}
    end.

disconnected({{?MODULE, _SerialClientNode},{command,Message}}, State) ->
    send_msg(Message, State),
    {next_state, connected, State}.

connected({{?MODULE, _SerialClientNode},{command,Message}}, State) ->
    send_msg(Message, State),
    {next_state, connected, State}.

handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
  {reply, ok, StateName, State}.

handle_info({_Pid, {command, [?SEND, Message]}}, connected, State) ->
    SerialClientNode = proplists:get_value(serial_client_node, State),
    gen_fsm:send_event({?MODULE, SerialClientNode},
                       {{?MODULE, node()},{command, Message}}),
    {next_state, connected, State};
handle_info({_Pid, {command, [?SEND, Message]}}, disconnected, State) ->
    SerialClientNode = proplists:get_value(serial_client_node, State),
    case alive_check(SerialClientNode) of
        true ->
            gen_fsm:send_event({?MODULE, SerialClientNode},
                               {{?MODULE, node()},{command, Message}}),
            {next_state, connected, State};
        false ->
            {next_state, disconnected, State}
    end;
%% @TODO: handle connect
handle_info({_Pid, {command, [?CONNECT]}}, StateName, State) ->
    {next_state, StateName, State};
%% @TODO : handle disconnect
handle_info({_Pid, {command, [?DISCONNECT]}}, StateName, State) ->
    {next_state, StateName, State};
%% @TODO : handle open
handle_info({_Pid, {command, [?OPEN, _TTY]}}, StateName, State) ->
    {next_state, StateName, State};
%% @TODO : handle close
handle_info({_Pid, {command, [?CLOSE]}}, StateName, State) ->
    {next_state, StateName, State};
%% @TODO : handle both in and out speed, can be handled in same clause
handle_info({_Pid, {command, [?SPEED, _NewInSpeed, " ", _NewOutSpeed,0]}},
            StateName, State) ->
    {next_state, StateName, State};
%% @TODO : handle parity
handle_info({_Pid, {command, [?PARITY_ODD]}}, StateName, State) ->
    {next_state, StateName, State};
%% @TODO : handle parity
handle_info({_Pid, {command, [?PARITY_EVEN]}}, StateName, State) ->
    {next_state, StateName, State};
%% @TODO : handle break
handle_info({_Pid, {command, [?BREAK]}}, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
alive_check(SerialClientNode) ->
    is_pid(rpc:call(SerialClientNode, erlang, whereis, [?MODULE])).

% @Todo: Add check for parent_pid?
% if its not available we crash and get an error?
send_msg(Message, State) ->
    ParentPid = proplists:get_value(parent_pid, State),
    ParentPid ! {whereis(?MODULE), {data, Message}}.
