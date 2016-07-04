%%%-------------------------------------------------------------------
%%% @doc
%%% This is the main module, which contains all API functions.
%%% It also implements the application behaviour
%%% @end
%%%-------------------------------------------------------------------
-module(netconf).
-behaviour(application).

%% Application callbacks
-export([start/0, start/2]).
-export([prep_stop/1]).
-export([stop/0, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%% @hidden
-spec(start(StartType :: normal | {takeover, node()} | {failover, node()},
    StartArgs :: term()) ->
  {ok, pid()} |
  {error, Reason :: term()}).
start(_StartType, _StartArgs) ->
  {ok, SupPid} = netconf_sup:start_link(),
  {ok, SSHPid} = start_ssh_daemon(),
  {ok, SupPid, SSHPid}.

%% @hidden
-spec(stop(pid()) -> term()).
stop(_Pid) ->
  ok.

%% @hidden
-spec(prep_stop(pid()) -> pid()).
prep_stop(SSHPid) ->
  ssh:stop_daemon(SSHPid),
  SSHPid.

%% @doc Starts `netconf' application.
-spec start() -> {ok, _} | {error, term()}.
start() -> application:ensure_all_started(netconf).

%% @doc Stops `netconf' application.
-spec stop() -> ok | {error, term()}.
stop() -> application:stop(netconf).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

start_ssh_daemon() ->
  {ok, Port} = application:get_env(netconf, port),
  Credentials = application:get_env(netconf, ssh_credentials, []),
  ssh:daemon(Port, [{system_dir, filename:join([code:priv_dir(netconf), "ssh"])}
          , {user_dir, filename:join([code:priv_dir(netconf), "ssh"])}
          , {shell, {unavailable, unavailable, []}}
          , {silently_accept_hosts, true}
          , {subsystems, [{"netconf", {netconf_ssh_channel, []}}]}
          , {user_passwords, Credentials}]).
