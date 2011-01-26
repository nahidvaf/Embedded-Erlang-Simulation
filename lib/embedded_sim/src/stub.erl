-module(stub).

-export([start_port/3]).

start_port(Mod, PortName, PortSettings) ->
    case os:getenv("EMBEDDED_ENV") of
        "sim"  ->
            Args = [{parent_pid, self()},
                    {portname, PortName},
                    {port_settings, PortSettings}],
            gen_fsm:start_link({local, Mod}, Mod, Args, []);
        "hard" ->
            open_port(PortName, PortSettings);
        false ->
            open_port(PortName, PortSettings)
    end.
