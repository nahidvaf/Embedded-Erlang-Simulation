-module(stub).

-export([start_port/3]).

start_port(Mod, PortName, PortSettings) ->
    case os:getenv("EMBEDDED_ENV") of
        "sim"  ->
            gen_fsm:start_link(Mod, [{PortName, PortSettings}], []);
        "hard" ->
            open_port(PortName, PortSettings);
        false ->
            open_port(PortName, PortSettings)
    end.
