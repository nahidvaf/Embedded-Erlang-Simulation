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

-export([start_server/2]).

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
%% @doc Starts a button stub or a gen server acting as a "driver" depending on OS
%% env var EMBEDDED_SIM
%% @end
%% ------------------------------------------------------------------------------
start_server(ServerMod, Args) ->
    stub:start_server(?MODULE, ServerMod, Args).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, released, Args}.

pushed({command, {push, _Time}}, State) ->
    {next_state, pushed, State};
pushed(change_state, State) ->
    send_msg(released, State),
    {next_state, released, State}.

released({command, {push, Time}}, State) ->
    send_msg(pushed, State),
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
    released({command, {push, Time}}, State);
handle_info({Pid, {command, state}}, StateName, State) ->
            Pid ! {self(), StateName},
            {next_state, StateName, State};
handle_info({Pid, {command, is_pushed}}, pushed, State) ->
    Pid ! {self(), true},
    {next_state, on, State};
handle_info({Pid, {command, is_pushed}}, released, State) ->
    Pid ! {self(), false},
    {next_state, on, State}.

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

send_msg(Message, State) ->
    ParentPid = proplists:get_value(parent_pid, State),
    ParentPid ! {self(), Message},
    MonitorServer = proplists:get_value(monitor_server, State),
    gen_server:cast(MonitorServer, {?MODULE, atom_to_list(Message)}).
