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

%% ------------------------------------------------------------------------------
%% @author Rickard Olsson <rickard@ooodev.com>
%% @author Reza Javaheri <reza@ooodev.com>
%% @doc Simulates button gen_server API "driver"
%% @end
%% ------------------------------------------------------------------------------

-module(button_stub).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_port/2]).

%% ------------------------------------------------------------------
%% gen_fsm State Exports
%% ------------------------------------------------------------------

-export([pushed/2, released/2]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4
        ]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------------------
%% @doc Starts a button stub or a port depending on OS env var EMBEDDED_SIM
%% @end
%% ------------------------------------------------------------------------------
start_port(PortName, PortSettings) ->
    stub:start_port(?MODULE, PortName, PortSettings).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, released, Args}.

pushed({command, {push, _Time}}, State) ->
    {next_state, pushed, State};
pushed(change_state, State) ->
    Message = <<0:64, 1:16/little, 276:16/little, 0:32/little, 0:1>>,
    send_msg(Message, State),
    {next_state, released, State}.

released({command, {push, Time}}, State) ->
    Message = <<0:64, 1:16/little, 276:16/little, 1:32/little, 0:1>>,
    send_msg(Message, State),
    gen_fsm:send_event_after(Time, change_state),
    {next_state, pushed, State};
released(change_state, State) ->
    {next_state, released, State}.

handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
  {reply, ok, StateName, State}.

%% Redirects call to pushed, purpose is to enable to both bang and use gen_fsm
handle_info({_Pid, {command, {push, _Time}}}, pushed, State) ->
    pushed({command, {push, _Time}}, State);
handle_info({_Pid, {command, {push, Time}}}, released, State) ->
    released({command, {push, Time}}, State).

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

send_msg(Message, State) ->
    ParentPid = proplists:get_value(parent_pid, State),
    ParentPid ! {self(), {data, Message}},

    % Soend message to monitor
    MonitorServer = proplists:get_value(monitor_server, State),
    MessageString = format(Message),
    gen_server:cast(MonitorServer, {?MODULE, MessageString}).

format(<<0:64, 1:16/little, 276:16/little, 0:32/little, 0:1>>) ->
    "released";
format(<<0:64, 1:16/little, 276:16/little, 1:32/little, 0:1>>) ->
    "pushed".
