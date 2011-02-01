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

-export([pushed/2, released/2, timeout/2]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4
        ]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_port(PortName, PortSettings) ->
    stub:start_port(?MODULE, PortName, PortSettings).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    {ok, released}.

pushed({command, push}, State) ->
    {next_state, pushed, State, 40}.

released({command, push}, State) ->
    {next_state, pushed, State, 40}.

timeout(_, State) ->
    {next_state, released, State}.

handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
  {reply, ok, StateName, State}.

handle_info({_Pid, {command, push}}, _StateName, State) ->
    {next_state, pushed, State, 40};
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

