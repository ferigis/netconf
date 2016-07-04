%%%-------------------------------------------------------------------
%%% @doc
%%% This module is a ssh_channel. It works as a ssh subsystem.
%%% This is the core of the netconf system.
%%% @end
%%%-------------------------------------------------------------------
-module(netconf_ssh_channel).

-behaviour(ssh_channel).

%% ssh_channel callbacks
-export([init/1,
  handle_msg/2,
  handle_call/3,
  handle_cast/2,
  handle_ssh_msg/2,
  code_change/3,
  terminate/2
]).

-define(DATA_TYPE_CODE, 0).

-record(state, {
  session_id :: integer(),
  device_module :: module()
}).

%%%===================================================================
%%% ssh_channel callbacks
%%%===================================================================

%% @private
init(_) ->
  {ok, DeviceModule} = application:get_env(netconf, device_module),
  {ok, #state{device_module = DeviceModule}}.

%% @private
handle_msg({ssh_channel_up, ChannelId, ConnRef}, #state{device_module = DeviceModule} = State) ->
  Capabilities = apply(DeviceModule, capabilities, []),
  {new_session_id, SessionId} = netconf_server:new_session_id(),
  Hello = netconf_message:hello(Capabilities, SessionId),
  ssh_connection:send(ConnRef, ChannelId, Hello),
  {ok, State#state{session_id = SessionId}};
handle_msg(_Message, State) ->
  {ok, State}.

handle_call(_Req,_From,State) ->
  {reply,ok,State}.

handle_cast(_Req,State) ->
  {reply,State}.

%% @private
handle_ssh_msg({ssh_cm, ConnRef, {data, ChannelId, ?DATA_TYPE_CODE, Data}}, State) ->
  process_msg(Data, ConnRef, ChannelId),
  {ok, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% @private
terminate(_Subsystem, _State) ->
  ok.

%%-----------------------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------------------
process_msg(<<>>, _ConnRef, _ChannelId) -> ok;
process_msg(Message, ConnRef, ChannelId) ->
  case (catch netconf_message:decode(Message)) of
    {'EXIT', _} ->
      %TODO check the version in order to send
      %TODO malformed error message
      ok;
    Decoded ->
      dispatch(Decoded, ConnRef, ChannelId)
  end.

dispatch({{hello, Attributes, Content}, Rest}, ConnRef, ChannelId) ->
  case validate_hello({hello, Attributes, Content}) of
    true -> ok;
    false -> close_channel
  end;
dispatch({{rpc, Attributes, Content}, Rest}, ConnRef, ChannelId) ->
  Error = netconf_error:operation_not_supported(),
  RpcReply = netconf_message:rpc_reply(Attributes, [Error]),
  ssh_connection:send(ConnRef, ChannelId, RpcReply),
  process_msg(Rest, ConnRef, ChannelId);
dispatch(Message, ConnRef, ChannelId) ->
  io:format("unsupported message~n~p~n", [Message]),
  ok.

validate_hello({hello, Attributes, [{capabilities, [], Capabilities}]}) ->
  true.