-module(ssh_common_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Common test
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

%% Tests
-export([new_session_id/1]).
-export([connection/1]).
-export([hello_test/1]).
-export([hello_test2/1]).

%% Macros
-define(OP_NOT_SUPPORTED_ERROR,
  [{'rpc-error',[{xmlns,"urn:ietf:params:xml:ns:netcolnf:base:1.0"}],
  [{'error-tag',[],["operation-not-supported"]},
  {'error-type',[],["protocol"]},
  {'error-severity',[],["error"]},
  {'error-info',[],["none"]}]}]).

-define(WRONG_HELLO, "<hello xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\">
  <capabilities><capability>urn:ietf:params:netconf:base:1.0</capability></capabilities>
  </hello>").


%%%===================================================================
%%% Common Test
%%%===================================================================

all() ->
  [new_session_id
    , connection
    , hello_test].

init_per_suite(Config) ->
  netconf:start(),
  Config.

end_per_suite(Config) ->
  netconf:stop(),
  Config.

%%%===================================================================
%%% Exported Tests Functions
%%%===================================================================

new_session_id(_Config) ->
  {new_session_id, 1} = netconf_server:new_session_id(),
  {new_session_id, 2} = netconf_server:new_session_id(),
  {new_session_id, 3} = netconf_server:new_session_id(),
  {new_session_id, 4} = netconf_server:new_session_id(),
  ok.

connection(_Config) ->
  {ok, Host} = application:get_env(netconf, host),
  {ok, Port} = application:get_env(netconf, port),
  {ok, _} = ct_netconfc:open([{ssh, Host}
    , {port, Port}
    , {user, "felipe"}, {password, "password"}]),
  {error, _} = ct_netconfc:open([{ssh, Host}
    , {port, Port}
    , {user, "felipe"}, {password, "wrong"}]).



hello_test(_Config) ->
  {ok, Host} = application:get_env(netconf, host),
  {ok, Port} = application:get_env(netconf, port),
  {ok, Handler} = ct_netconfc:open([{ssh, Host}
                      , {port, Port}
                      , {user, "felipe"}, {password, "password"}
                      , {user_dir, "../../test/ssh"}]),
  ?OP_NOT_SUPPORTED_ERROR = ct_netconfc:send_rpc(Handler, "<not-supported-method />"),
  ok.

hello_test2(_Config) ->
  {ok, Host} = application:get_env(netconf, host),
  {ok, Port} = application:get_env(netconf, port),
  {ok, Handler} = ct_netconfc:only_open([{ssh, Host}
    , {port, Port}
    , {user_dir, "../../test/ssh"}]),
  H = ct_netconfc:send(Handler, ?WRONG_HELLO),
  ok.
