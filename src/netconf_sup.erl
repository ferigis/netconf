%%%-------------------------------------------------------------------
%%% @doc
%%% The application supervisor
%%% @end
%%%-------------------------------------------------------------------
-module(netconf_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
%% @hidden
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @hidden
init([]) ->
  SupFlags = {one_for_one, 1000, 3600},

  Restart = permanent,
  Shutdown = 2000,
  Type = worker,

  AChild = {server, {netconf_server, start_link, []},
    Restart, Shutdown, Type, [netconf_server]},

  {ok, {SupFlags, [AChild]}}.
