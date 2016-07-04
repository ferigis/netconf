%%%-------------------------------------------------------------------
%%% @doc
%%% This module handles all the netconf messages
%%% @end
%%%-------------------------------------------------------------------
-module(netconf_message).

-include_lib("xmerl/include/xmerl.hrl").

-export([hello/2]).
-export([rpc_reply/2]).
-export([encode/1]).
-export([decode/1]).

-define(PROLOG, {prolog, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"}).
-define(END, <<"]]>]]>">>).

%%%===================================================================
%%% Types & Macros
%%%===================================================================

-type tag() :: atom().
-type attribute() :: {atom(), integer() | atom() | string() | iolist()}.
-type xml_element() :: {tag(), [attribute()], [xml_element()]} | atom() | string() | iolist().

%% Exported types
-export_types([xml_element/0]).

-spec hello(list(string()), integer()) -> iolist().
hello(Capabilities, SessionId) ->
  CapabilitiesFormated = {capabilities, [{capability, [C]} || C <- Capabilities]},
  Hello = [{hello, [{xmlns, "urn:ietf:params:xml:ns:netcolnf:base:1.0"}],
              [CapabilitiesFormated, {'session-id', [integer_to_list(SessionId)]}]
          }],
  encode(Hello).

-spec rpc_reply([attribute()], [xml_element()]) -> iolist().
rpc_reply(Attributes, Elements) ->
  Reply = [{'rpc-reply', Attributes, Elements}],
  encode(Reply).

-spec encode(xml_element()) -> iolist().
encode(Element) ->
  Xml = xmerl:export_simple(Element, xmerl_xml, [?PROLOG]),
  Binary = list_to_binary(Xml),
  [Binary, ?END].


-spec decode([binary()]) -> {xml_element(), [binary()]}.
decode(EncodedMessage) ->
  [FirstMessage, Rest] = re:split(EncodedMessage, ?END),
  {ScannedXML, _Rest} = xmerl_scan:string(binary_to_list(FirstMessage), [{quiet, true}]),
  {convert_element(ScannedXML), Rest}.

%%-----------------------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------------------

convert_element(#xmlElement{name = Name,
                attributes = Attrs,
                content = Content}) ->
  {Name, [convert_attribute(At) || At <- Attrs], [convert_element(El) || El <- Content]};
convert_element(#xmlText{value = Value}) -> Value;
convert_element(Element) -> Element.
convert_attribute(#xmlAttribute{name = Name, value = Value}) -> {Name, Value}.
