-module(serial_demo_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    serial_demo_sup:start_link().

stop(_State) ->
    ok.
