%% -----------------------------------------------------------------
%% @author Rickard Olsson <rickard@ooodev.com>
%% @author Reza Javaheri <reza@ooodev.com>
%% @doc Simulates led gen_server API "driver"
%% @end
%% ------------------------------------------------------------------

-module(led_stub).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_server/2]).

%% ------------------------------------------------------------------
%% gen_fsm State Exports
%% ------------------------------------------------------------------

-export([on/2, off/2]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4
        ]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------------------
%% @doc Starts a led stub or a gen server acting as a "driver" depending on OS
%% env var EMBEDDED_SIM
%% @end
%% ------------------------------------------------------------------------------
start_server(ServerMod, Args) ->
    stub:start_server(?MODULE, ServerMod, Args).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, off, Args}.

on({command, turn_on}, State) ->
    {next_state, on, State};
on({command, turn_off}, State) ->
    {next_state, off, State}.

off({command, turn_on}, State) ->
    {next_state, on, State};
off({command, turn_off}, State) ->
    {next_state, off, State}.

handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
  {reply, ok, StateName, State}.

handle_info({_Pid, {command, turn_on}}, _StateName, State) ->
    send_msg(on, State),
    {next_state, on, State};
handle_info({_Pid, {command, turn_off}}, _StateName, State) ->
    send_msg(off, State),
    {next_state, off, State};
handle_info({Pid, {command, brightness}}, StateName, State) ->
    Pid ! {self(), StateName},
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
send_msg(Message, State) ->
    MonitorServer = proplists:get_value(monitor_server, State),
    gen_server:cast(MonitorServer,
                    {?MODULE, atom_to_list(Message)}).
