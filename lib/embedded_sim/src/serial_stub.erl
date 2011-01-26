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
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_port(PortName, PortSettings) ->
    stub:start_port(?MODULE, PortName, PortSettings).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------


init(Args) ->
    SerialClientNode = application:gen_env(embedded_sim, serial_client_node),
    {ok, disconnected, [{serial_client_node, SerialClientNode}|Args]}.

disconnected({{?MODULE, SerialClientNode},{command,Message}}, State) ->
    send_msg(Message, State),
    {next_state, connected, State}.

connected({{?MODULE, SerialClientNode},{command,Message}}, State) ->
    send_msg(Message, State),
    {next_state, connected, State}.

handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
  {reply, ok, StateName, State}.

handle_info({Pid, {command, Message}}, connected, State) ->
    SerialClientNode = proplists:get_value(serial_client_node, State),
    io:format("sending command ~w to ~w ~n",
              [Message, SerialClientNode]),
    gen_fsm:send_event({?MODULE, SerialClientNode},
                       {{?MODULE, node()},{command, Message}}),
    {next_state, connected, State};


handle_info({Pid, {command, Message}}, disconnected, State) ->
    SerialClientNode = proplists:get_value(serial_client_node, State),
    case alive_check(SerialClientNode) of
        true ->
            io:format("sending command ~w to ~w ~n",
                      [Message, SerialClientNode]),
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
    case rpc:call(SerialClientNode, erlang, whereis, [?MODULE]) of
        undefined ->
            false;
        _Other ->
            true
    end.

send_msg(Message, State) ->
    ParentPid = proplists:get_value(parent_pid, State),
    ParentPid ! {whereis(?MODULE), {data, Message}}.
