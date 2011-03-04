-module(rec_demo).

-export([record/0, record/2, replay/0, replay/1, replay/2, test/0]).

record() ->
    RecDir = "../../rec_logs",
    Num = inc_counter(RecDir),
    DriverOutput = io_lib:format("~s/~s_~B.log", [RecDir, "beagle_driver", Num]),
    AppOutput = io_lib:format("~s/~s_~B.log", [RecDir, "beagle_app", Num]),
    record(DriverOutput, AppOutput).

record(DriverOutput, AppOutput) ->
    serial_demo:start(),
    rec:start(),
    timer:sleep(100),
    rec:add_process(beagle_button, DriverOutput, [port]),
    rec:add_process(serial_demo, AppOutput, [send]).

replay() ->
    % For replaying on beagleboard
    %RecDir = "../../rec_logs",
    % For replaying on simualtor
    RecDir = "../../../rec_logs",
    replay(RecDir).

replay(RecDir) ->
    Num = get_counter(RecDir),
    DriverOutput = io_lib:format("~s/~s_~B.log", [RecDir, "beagle_driver", Num]),
    AppOutput = io_lib:format("~s/~s_~B.log", [RecDir, "beagle_replay_app", Num]),
    replay(DriverOutput, AppOutput).


replay(DriverOutput, AppOutput) ->
    serial_demo:start(),
    timer:sleep(100),
    rec:start(),
    io:format("DriverOutput: ~p AppOutput: ~p ~n", [DriverOutput, AppOutput]),
    rec:add_process(serial_demo, AppOutput, [send]),
    rec:replay(DriverOutput).

get_counter(RecDir) ->
    CounterFile = RecDir ++ "/rec_counter",
    filelib:ensure_dir(RecDir),
    case file:consult(CounterFile) of
        {ok, []} ->
            1;
        {ok, [PrevNum]} when is_integer(PrevNum) ->
            PrevNum;
        _ ->
            1
    end.

inc_counter(RecDir) ->
    CounterFile = RecDir ++ "/rec_counter",
    filelib:ensure_dir(RecDir),
    NewNum = get_counter(RecDir) + 1,
    file:write_file(CounterFile, io_lib:format("~w.", [NewNum])),
    NewNum.

test() ->
    receive _Other -> ok end,
    test().
