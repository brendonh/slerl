%%%-------------------------------------------------------------------
%%% File    : slerl_sim.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : UDP Simulator connection
%%%
%%% Created :  7 Feb 2010 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(slerl_sim).

-behaviour(gen_fsm).

%% API
-export([start_link/2]).

%% gen_fsm callbacks
-export([init/1, state_name/2, state_name/3, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state, {}).


%%====================================================================
%% API
%%====================================================================

start_link(Info, Messages) ->
    gen_fsm:start_link(?MODULE, [], []).


%%====================================================================
%% gen_fsm callbacks
%%====================================================================

init([]) ->
    {ok, state_name, #state{}}.


state_name(_Event, State) ->
    {next_state, state_name, State}.


state_name(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, state_name, State}.


handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.


handle_sync_event(Event, From, StateName, State) ->
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
