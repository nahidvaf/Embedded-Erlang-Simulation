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
                                             term_to_binary(P),
                                             'send',
                                             term_to_binary(M),
                                             term_to_binary(To)]);
                                       {trace, P, 'receive', M} ->
                                             io_lib:format(
                                           "{trace,{delay, ~p},{pid, ~p},{type, ~p}" ++
                                            ",{msg, ~p}}.~n",
                                            [TimeStamp,
                                             term_to_binary(P),
                                             'receive',
                                             term_to_binary(M)])
                                        end,
                        ok = file:write_file(FileName, TraceStr, [append]),
                        Now
                end, now()}).

add_process(Pid) ->
    dbg:p(Pid, [m]).

stop() ->
    dbg:stop_clear().
