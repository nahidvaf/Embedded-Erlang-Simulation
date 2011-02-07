-module(stub).

-export([start/1, start_port/3, start_server/3]).

-define(GEN_SIM(Mod,Args),
        {"sim", {gen_fsm,start_link,[{local, Mod},Mod,Args,[]]}}).
-define(GEN_HARD(Args),
        {"hard",{erlang,open_port,Args}}).

start_server(Mod, ServerMod, Args) ->
    SimArgs = [{parent_pid, self()}, {monitor_server, stub_monitor}],
    Setup = [{"hard", {ServerMod, start_link, Args}}, ?GEN_SIM(Mod, SimArgs)],
    start(Setup).

start_port(Mod, PortName, PortSettings) ->
    SimArgs = [{parent_pid, self()}, {monitor_server, stub_monitor}],
    Setup = [?GEN_HARD([PortName, PortSettings]), ?GEN_SIM(Mod, SimArgs)],
    start(Setup).

start(Setup) ->
    init(Setup, os:getenv("EMBEDDED_ENV")).

init(Setup, false) ->
     init(Setup, "hard");
init(Setup, Env) ->
    case proplists:get_value(Env, Setup) of
        undefined ->
            {error, no_setup};
        {M, F, A} ->
            case erlang:apply(M, F, A) of
                {ok, Stub} ->
                    {ok, Stub};
                Port ->
                    {ok, Port}
            end
    end.
