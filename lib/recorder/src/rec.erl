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
%% @doc Records and replays messages
%% @end
%% ------------------------------------------------------------------------------

-module(rec).

-export([start/0, start/1, start/2, add_process/3, stop/0, replay/1, replay/2]).

-export([config_server/1]).

-type error() :: term().
-type filename() :: string().
-type msg_option() :: port | 'receive' | send.

% Recorder
% TODO: add processes in config at start
-spec (start/0 :: () -> {ok, pid()} | {error, error()}).
start() ->
    start([]).

-spec (start/1 :: ([{pid(), filename(), [msg_option()]}|
                    {rest, filename(), [msg_option()]}]) ->
              {ok, pid()} | {error, error()}).
start(Config) ->
   start(Config, []).

-spec (start/2 :: ([{pid(), filename(), [msg_option()]}],
                    [v]) ->
              {ok, pid()} | {error, error()}).
start(Config, Options) ->
    % init options
    set_env(verbose, v, Options),

    init(Config),
    % TODO: store prevTimestamp and pid in accumulator,
    % record_msg should return pid, and if validation fails, prevTimestamp
    % for that pid should not be updatedg
    HandlerFun = fun(Msg, PrevTimeStamp) ->
                         Now = now(),
                         record_msg(Msg, PrevTimeStamp, Now),
                         Now
                 end,
    dbg:tracer(process,{HandlerFun, now()}).

init(Args) ->
    Pid = spawn_link(?MODULE, config_server, [Args]),
    true = register(rec_config_server, Pid).

config_server(Config) ->
    NewConfig = receive
                    {Pid, get_config} ->
                        Pid ! {config, Config},
                        Config;
                    {add, ConfigElement} ->
                        [ConfigElement | Config]
                end,
    config_server(NewConfig).

get_config() ->
    rec_config_server ! {self(), get_config},
    receive
        {config, Config} ->
            Config
    after 1000 ->
            exit(config_server_not_available)
    end.

% Add process with dbg:p even if it is already added
add_process(Pid, FileName, MsgOptions) when is_pid(Pid) ->
    add_process(get_reg_name(Pid), FileName, MsgOptions);
add_process(Process, FileName, MsgOptions) ->
    case lists:keymember(Process, 1, get_config()) of
        true ->
            ok;
        false ->
            rec_config_server ! {add, {Process, FileName, MsgOptions}}
    end,
    dbg:p(Process, [m]).

stop() ->
    dbg:stop_clear().

%
% Internal Functions
%
record_msg(Msg, PrevTimeStamp, Now) ->
    TimeStamp = timer:now_diff(Now, PrevTimeStamp),
    Config = get_config(),
    case validate_msg(Msg, Config) of
        true ->
            d_print("message was valid: ~w~n", [Msg]),
            {Process, MsgStr} = process_msg(Msg, TimeStamp),
            FileName = get_filename(Process, Config),
            d_print("Received ~w on process ~w after ~w uSec" ++
                    "writing to file ~s ~n",
                    [MsgStr, Process, TimeStamp, FileName]),
            ok = file:write_file(FileName, MsgStr, [append]);
        _Else ->
            ok
    end.

% Extract options for a process from config, third element is options
extract_options({trace, Pid, Type, Msg, _To}, Config) when is_pid(Pid) ->
    extract_options({trace, Pid, Type, Msg}, Config);
extract_options({trace, Pid, _Type, _Msg}, Config)  when is_pid(Pid) ->
    case lists:keyfind(get_reg_name(Pid), 1, Config) of
        {_Process, _Filename, MsgOptions} ->
            MsgOptions;
        false ->
            exit(process_not_in_config);
        _Other ->
            exit(config_server_corrupted)
    end.

get_filename(Process, Config) ->
    case lists:keyfind(Process, 1, Config) of
        {_Process, FileName, _MsgOptions} ->
            FileName;
        false ->
            exit(process_not_in_config);
        _Other ->
            exit(config_server_corrupted)
    end.

validate_msg(Msg, Config) ->
    case extract_options(Msg, Config) of
        false ->
            false;
        Options ->
            ZippedOptions =
                lists:zip(lists:duplicate(length(Options), Msg), Options),
            lists:any(fun validate/1, ZippedOptions)
    end.

validate({{trace, _Pid, 'receive', _Msg}, 'receive'}) ->
    true;
validate({{trace, _Pid, 'send', _Msg, _To}, 'send'}) ->
    true;
validate({{trace, _Pid, 'receive', {Port, _}}, port}) when is_port(Port) ->
    true;
validate(_AnythingElse) ->
    false.

get_reg_name(Pid) when (is_pid(Pid)) ->
    PInfo = process_info(Pid),
    PName = proplists:get_value(registered_name, PInfo),
    case PName of
        undefined ->
            exit(recording_pid_not_registered);
        Name ->
            Name
    end;
get_reg_name(Process) ->
    Process.

process_msg({trace, P, 'send', M, To}, TimeStamp) ->
    Process = get_reg_name(P),
    {Process, io_lib:format(
          "{trace,{delay, ~w},{pid, ~w},{type, ~w}" ++
          ",{msg, ~w},{to, ~w}}.~n",
          [TimeStamp,
           Process,
           'send',
           M,
           get_reg_name(To)])};
process_msg({trace, P, 'receive', {Port, M}}, TimeStamp) when (is_port(Port)) ->
    Process = get_reg_name(P),
    {Process, io_lib:format(
          "{trace,{delay, ~w},{pid, ~w},{type, ~w}" ++
          ",{msg, ~w}}.~n",
          [TimeStamp,
           Process,
           'receive',
           {port, M}])};
process_msg({trace, P, 'receive', M}, TimeStamp) ->
    Process = get_reg_name(P),
    {Process, io_lib:format(
          "{trace,{delay, ~w},{pid, ~w},{type, ~w}" ++
          ",{msg, ~w}}.~n",
          [TimeStamp,
           Process,
           'receive',
           M])}.

%
% Replayer
%
replay(LogFile) ->
    replay(LogFile, []).

replay(LogFile, Options) ->
    {ok, MessageList} = file:consult(LogFile),
    set_env(verbose, v, Options),
    lists:foreach(fun apply_message/1, MessageList).

%
% Internal functions
%
apply_message({trace, {delay, Delay}, {pid, Process}, {type, 'receive'},
               {msg, Message}}) ->
    send_message(Delay, Process, Message);
apply_message(_) ->
    ok.

send_message(Delay, Process, Message)->
    d_print("~n Sleeping for ~p msec ~n", [Delay div 1000]),
    timer:sleep(Delay div 1000),
    d_print("~n Send msg ~p", [Message]),
    Process ! Message.

d_print(Message, Args) ->
    case application:get_env(recorder, verbose) of
        {ok, true} ->
            io:format(Message, Args);
        _ ->
            ok
    end.

% Internal functions for recorder and replayer
set_env(Env, Alias, Options) ->
    case lists:member(Alias, Options) of
        true ->
            application:set_env(recorder, Env, true);
        _ ->
           ok
    end.
