-module(replayer).
-compile(export_all).
-export([start/1]).

start(LogFile) ->
    {ok, MessageList} = file:consult(LogFile),
    lists:foreach(fun apply_message/1, MessageList).

apply_message({trace, {delay, Delay}, {pid, Process}, {type, 'receive'},
               {msg, Message}}) ->
    send_message(Delay, Process, Message);
apply_message(_) ->
    ok.

send_message(Delay, Process, Message)->
    timer:sleep(Delay div 1000),
    Process ! Message.


