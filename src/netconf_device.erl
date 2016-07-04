%%%-------------------------------------------------------------------
%%% @doc
%%% This is module is the device behaviour.
%%% @end
%%%-------------------------------------------------------------------
-module(netconf_device).

-callback capabilities() -> []|list(string()).
