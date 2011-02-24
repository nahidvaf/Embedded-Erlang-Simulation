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

-export([start/2, add_process/2, stop/0]).

-export([config_server/1]).

-type error() :: term().
-type filename() :: string().

-spec (start/2 :: ([{pid(), filename()}|{rest, filename()}],
                   [port]|[]) ->
              {ok, pid()} | {error, error()}).
start(Config, Options) ->
    init(Config),
    HandlerFun = fun(Msg, PrevTimeStamp) ->
                         Now = now(),
                         record_msg(Msg, PrevTimeStamp, Now, Options),
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

add_process(Pid, FileName) when (is_pid(Pid)) ->
    rec_config_server ! {add, {get_reg_name(Pid), FileName}},
    dbg:p(Pid, [m]);

add_process(Process, FileName) ->
    rec_config_server ! {add, {Process, FileName}},
    dbg:p(Process, [m]).

stop() ->
    dbg:stop_clear().

%
% Internal Functions
%
record_msg(Msg, PrevTimeStamp, Now, Options) ->
    TimeStamp = timer:now_diff(Now, PrevTimeStamp),
    case validate_msg(Msg, Options) of
        true ->
            {Process, MsgStr} = process_msg(Msg, TimeStamp),
            FileName = get_filename(Process, get_config()),
            ok = file:write_file(FileName, MsgStr, [append]);
        _Else ->
            ok
    end.

get_filename(Process, Config) ->

    Rest = proplists:get_value(rest, Config, "rec.log"),
    proplists:get_value(Process, Config, Rest).


validate_msg(Msg, Options) ->
    ZippedOptions =
        lists:zip(lists:duplicate(length(Options), Msg), Options),
    lists:any(fun validate/1, ZippedOptions).

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
