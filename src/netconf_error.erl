%%%-------------------------------------------------------------------
%%% @doc
%%% This module represents all the errors we can handle on netconf.
%%% @end
%%%-------------------------------------------------------------------
-module(netconf_error).

%% API
-export([operation_not_supported/0]).

%%%===================================================================
%%% Supported Errors
%%%===================================================================

-spec operation_not_supported() -> netconf_message:xml_element().
operation_not_supported() ->
  error("operation-not-supported", "protocol", "none").

%%%===================================================================
%%% Internal Functions
%%%===================================================================

-spec error(string(), string(), string() | netconf_message:xml_element()) -> netconf_message:xml_element().
error(Tag, Type, Info) ->
  Elements = [{'error-tag', [Tag]}
              , {'error-type', [Type]}
              , {'error-severity', ["error"]}
              , {'error-info', [Info]}],
  {'rpc-error', [{xmlns, "urn:ietf:params:xml:ns:netcolnf:base:1.0"}], Elements}.
