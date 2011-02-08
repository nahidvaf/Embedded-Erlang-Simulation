%% -----------------------------------------------------------------
%% @author Rickard Olsson <rickard@oodev.com>
%% @author Reza Javaheri <reza@ooodev.com>
%% ------------------------------------------------------------------

-module(embedded_sim_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    embedded_sim_sup:start_link().

stop(_State) ->
    ok.
