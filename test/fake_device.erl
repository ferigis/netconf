-module(fake_device).

-behaviour(netconf_device).

%% API
-export([capabilities/0]).

capabilities() ->
  ["urn:ietf:params:netconf:base:1.0"
    , "urn:ietf:params:netconf:capability:writable-running:1.0"].
