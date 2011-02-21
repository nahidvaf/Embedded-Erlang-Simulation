-module(rec).
-compile(export_all).

start(FileName) ->
    dbg:tracer(process,
               {fun(Msg, PrevTimeStamp) ->
                        Now = now(),
                        TimeStamp = timer:now_diff(Now, PrevTimeStamp),
                        TraceStr = case Msg of
                                       {trace, P, 'send', M, To} ->
                                          io_lib:format(
                                           "{trace,{delay, ~p},{pid, ~p},{type, ~p}" ++
                                            ",{msg, ~p},{to, ~p}}.~n",
                                            [TimeStamp,
                                             get_reg_name(P),
                                             'send',
                                             M,
                                             get_reg_name(To)]);
                                       {trace, P, 'receive', M} ->
                                             io_lib:format(
                                           "{trace,{delay, ~p},{pid, ~p},{type, ~p}" ++
                                            ",{msg, ~p}}.~n",
                                            [TimeStamp,
                                             get_reg_name(P),
                                             'receive',
                                             M])
                                        end,
                        ok = file:write_file(FileName, TraceStr, [append]),
                        Now
                end, now()}).

add_process(Pid) ->
    dbg:p(Pid, [m]).

stop() ->
    dbg:stop_clear().

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
