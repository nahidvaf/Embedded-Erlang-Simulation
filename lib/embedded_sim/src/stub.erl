-module(stub).

-export([start_port/1, start_port/3, start_server/3]).

-define(GEN_SIM(Mod,Args),
        {"sim", {gen_fsm,start_link,[{local, Mod},Mod,Args,[]]}}).
-define(GEN_HARD(Args),
        {"hard",{erlang,port,Args}}).

start_server(Mod, ServerMod, Args) ->
    SimArgs = [{parent_pid, self()}],
    Setup = [{"hard", {ServerMod, start_link, Args}}, ?GEN_SIM(Mod, SimArgs)],
    start_port(Setup).

start_port(Mod, PortName, PortSettings) ->
    SimArgs = [{parent_pid, self()}],
    Setup = [?GEN_HARD([PortName, PortSettings]), ?GEN_SIM(Mod, SimArgs)],
    start_port(Setup).

start_port(Setup) ->
    init_port(Setup, os:getenv("EMBEDDED_ENV")).

init_port(Setup, false) ->
     init_port(Setup, "hard");
init_port(Setup, Env) ->
    case proplists:get_value(Env, Setup) of
        undefined ->
            {error, no_setup};
        {M, F, A} ->
            erlang:apply(M, F, A)
    end.
