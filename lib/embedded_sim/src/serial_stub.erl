-module(serial_stub).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).

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

start_port(PortName, PortSettings) ->
    stub:start_port(?MODULE, PortName, PortSettings).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

% Todo: {ok, SerialClientNode} was just SerialClientNode before
% This caused an error that was tricky to see at first, should we
% case if we get a good return and else crash or is that defensive porgrammng?
init(Args) ->
    {ok, SerialClientNode} = application:get_env(embedded_sim,
                                                 serial_client_node),

    {ok, disconnected, [{serial_client_node, SerialClientNode}|Args]}.

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

handle_info({_Pid, {command, Message}}, connected, State) ->
    SerialClientNode = proplists:get_value(serial_client_node, State),
    gen_fsm:send_event({?MODULE, SerialClientNode},
                       {{?MODULE, node()},{command, Message}}),
    {next_state, connected, State};


handle_info({_Pid, {command, Message}}, disconnected, State) ->
    SerialClientNode = proplists:get_value(serial_client_node, State),
    case alive_check(SerialClientNode) of
        true ->
            gen_fsm:send_event({?MODULE, SerialClientNode},
                               {{?MODULE, node()},{command, Message}}),
            {next_state, connected, State};
        false ->
            {next_state, disconnected, State}
    end.

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
alive_check(SerialClientNode) ->
    is_pid(rpc:call(SerialClientNode, erlang, whereis, [?MODULE])).

% Todo: Add check for parent_pid?
% if its not available we crash and get weird error?
send_msg(Message, State) ->
    ParentPid = proplists:get_value(parent_pid, State),
    ParentPid ! {whereis(?MODULE), {data, Message}}.
