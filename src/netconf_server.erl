%%%-------------------------------------------------------------------
%%% @doc
%%% This module is a gen server. It manages the session ids.
%%% @end
%%%-------------------------------------------------------------------
-module(netconf_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([new_session_id/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(ETS, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%% @hidden
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

new_session_id() ->
  gen_server:call(?SERVER, new_session_id).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @hidden
init([]) ->
  ?ETS = ets:new(?ETS, [set, named_table, {read_concurrency, true}]),
  true = ets:insert(?ETS, {session_id, 0}),
  {ok, #{}}.

handle_call(new_session_id, _from, State) ->
  NewSessionId = ets:update_counter(?ETS, session_id, 1),
  {reply, {new_session_id, NewSessionId}, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
