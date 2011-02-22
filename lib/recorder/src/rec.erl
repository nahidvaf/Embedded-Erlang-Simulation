-module(rec).

-export([start/2, add_process/1, stop/0]).

start(FileName, Options) ->
    HandlerFun = fun(Msg, PrevTimeStamp) ->
                         Now = now(),
                         record_msg(FileName, Msg, PrevTimeStamp, Now, Options),
                         Now
                 end,
    dbg:tracer(process,{HandlerFun, now()}).

add_process(Pid) ->
    dbg:p(Pid, [m]).

stop() ->
    dbg:stop_clear().

%
% Internal Functions
%
record_msg(FileName, Msg, PrevTimeStamp, Now, Options) ->
    TimeStamp = timer:now_diff(Now, PrevTimeStamp),
    case validate_msg(Msg, Options) of
        true ->
            ok = file:write_file(FileName, format_msg(Msg, TimeStamp), [append]);
        _Else ->
            ok
    end.

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

format_msg({trace, P, 'send', M, To}, TimeStamp) ->
    io_lib:format(
              "{trace,{delay, ~w},{pid, ~w},{type, ~w}" ++
              ",{msg, ~w},{to, ~w}}.~n",
              [TimeStamp,
               get_reg_name(P),
               'send',
               M,
               get_reg_name(To)]);
format_msg({trace, P, 'receive', {Port, M}}, TimeStamp) when (is_port(Port)) ->
    io_lib:format(
              "{trace,{delay, ~w},{pid, ~w},{type, ~w}" ++
              ",{msg, ~w}}.~n",
              [TimeStamp,
               get_reg_name(P),
               'receive',
               {port, M}]);
format_msg({trace, P, 'receive', M}, TimeStamp) ->
    io_lib:format(
              "{trace,{delay, ~w},{pid, ~w},{type, ~w}" ++
              ",{msg, ~w}}.~n",
              [TimeStamp,
               get_reg_name(P),
               'receive',
               M]).
