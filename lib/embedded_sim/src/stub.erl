%% ------------------------------------------------------------------------------
%% @author Rickard Olsson <rickard@ooodev.com>
%% @author Reza Javaheri <reza@ooodev.com>
%% @doc Stateless module that given the needed settings opens a port or starts
%% a server based on the os environmental variable "EMBEDDED_ENV".
%% When EMBEDDED_ENV is set to the value of "sim" the simulated
%% environment will be used otherwise if it is omitted or the value "hard" is set
%% the real hardware will be used.
%% @end
%% ------------------------------------------------------------------------------

-module(stub).

%% Interface exports
-export([start/1, start_port/3, start_server/3]).

%% Convenient macros for creating common setups for "sim" and "hard"
-define(GEN_SIM(Mod,Args),
        {"sim", {gen_fsm,start_link,[{local, Mod},Mod,Args,[]]}}).
-define(GEN_HARD(Args),
        {"hard",{erlang,open_port,Args}}).

%% ------------------------------------------------------------------------------
%% @doc Convenient function to start a normal server interfacing a device and
%% gen_fsm as simulator
%% @end
%% ------------------------------------------------------------------------------
-spec(start_server/3 :: (module(), function(), [term()])
      ->  {ok, pid()} |
          {ok, port()} |
          {error, no_setup} |
          {error, no_pid_or_port}).
start_server(Mod, ServerMod, Args) ->
    SimArgs = [{parent_pid, self()}, {monitor_server, stub_monitor}],
    Setup = [{"hard", {ServerMod, start_link, Args}}, ?GEN_SIM(Mod, SimArgs)],
    start(Setup).

%% ------------------------------------------------------------------------------
%% @doc Convenient function with a setup for port and gen_fsm as simulator
%% @end
%% ------------------------------------------------------------------------------
-spec(start_port/3 :: (module(), {spawn, string()}, [term()]) ->
             {ok, pid()} |
             {ok, port()} |
             {error, no_setup} |
             {error, no_pid_or_port}).
start_port(Mod, PortName, PortSettings) ->
    SimArgs = [{parent_pid, self()}, {monitor_server, stub_monitor}],
    Setup = [?GEN_HARD([PortName, PortSettings]), ?GEN_SIM(Mod, SimArgs)],
    start(Setup).

%% ------------------------------------------------------------------------------
%% @doc Starts stub or port depending on setup
%% @end
%% ------------------------------------------------------------------------------
-type env_state() :: string().
-type sim_conf() :: [{env_state(), mfa()}].
-spec(start/1 :: (sim_conf()) -> {ok, pid()} |
                                 {ok, port()} |
                                 {error, no_setup} |
                                 {error, no_pid_or_port}).
start(Setup) ->
    init(Setup, os:getenv("EMBEDDED_ENV")).

%--------------------------------------------------------------------------------
% Internal Functions
%--------------------------------------------------------------------------------

init(Setup, false) ->
     init(Setup, "hard");
init(Setup, Env) ->
    case proplists:get_value(Env, Setup) of
        undefined ->
            {error, no_setup};
        {M, F, A} ->
            case erlang:apply(M, F, A) of
                {ok, Stub} when (is_pid(Stub)) ->
                    {ok, Stub};
                Port when (is_port(Port)) ->
                    {ok, Port};
                _Else ->
                    {error, no_pid_or_port}
            end
    end.
