%%%-------------------------------------------------------------------
%%% File    : slerl_sim.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : High-level sim connection management
%%%
%%% Created : 10 Feb 2010 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(slerl_sim).

-behaviour(gen_fsm).

-include("slerl.hrl").
-include("slerl_util.hrl").

%% API
-export([start_link/2]).

%% gen_fsm callbacks
-export([init/1, 
         uninitialized/2, uninitialized/3, 
         connecting/2, connecting/3, 
         handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state, {
  name,
  simInfo,
  connKey
}).

%%====================================================================
%% API
%%====================================================================

start_link(Name, SimInfo) ->
    gen_fsm:start_link(?MODULE, [Name, SimInfo], []).


%%====================================================================
%% gen_fsm callbacks
%%====================================================================

init([Name, SimInfo]) ->
    ?DBG({sim_starting, SimInfo#sim.ip, SimInfo#sim.port}),

    State = #state{
      name=Name,
      simInfo=SimInfo,
      connKey={udp, SimInfo#sim.ip, SimInfo#sim.port}
     },

    ets:insert(Name, {{sim, SimInfo#sim.ip, SimInfo#sim.port}, self()}),

    {ok, uninitialized, State}.



uninitialized(start_connect, State) ->
    start_connect(State),
    {next_state, connecting, State}.

uninitialized(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, uninitialized, State}.



connecting(Event, State) ->
    ?DBG({connecting_event, Event}),
    {next_state, connecting, State}.

connecting(_Event, _From, State) ->
    {reply, ok, connecting, State}.


handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.


handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.


terminate(_Reason, _StateName, _State) ->
    ok.


code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
    
start_connect(State) ->
    ?DBG(starting_connect),
    gen_server:cast(get_conn(State), start_connect).


get_conn(#state{name=Name, connKey=ConnKey}) ->
    ets:lookup_element(Name, ConnKey, 2).
