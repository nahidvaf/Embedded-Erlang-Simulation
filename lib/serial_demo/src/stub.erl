-module(stub).

-export([start_port/3]).

start_port(Mod, PortName, PortSettings) ->
    case os:getenv("EMBEDDED_ENV") of
        "sim"  ->
            gen_server:start_link(Mod, [{PortName, PortSettings}], []);
        "hard" ->
            io:format("open_port(PortName, PortSettings);");
        false ->
            io:format("open_port(PortName, PortSettings);")
    end.
