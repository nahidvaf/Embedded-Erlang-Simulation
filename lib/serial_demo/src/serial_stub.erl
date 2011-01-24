-module(serial_stub).

-behaviour(gen_server).

%% API functions
-export([start_port/2]).

%% Callbacks
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).

start_port(PortName, PortSettings) ->
    stub:start_port(?MODULE, PortName, PortSettings).

init(Args) ->
    {ok, Args}.

handle_info({_Pid, {command, Message}}, State) ->
    io:format("command ~w", [Message]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
