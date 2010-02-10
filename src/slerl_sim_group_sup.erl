%%%-------------------------------------------------------------------
%%% File    : slerl_sim_group_sup.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : Collection of processes for a single sim connection
%%%
%%% Created :  7 Feb 2010 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(slerl_sim_group_sup).

-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).


%%====================================================================
%% API functions
%%====================================================================

start_link(Bot, SimInfo) ->
    supervisor:start_link(?MODULE, [Bot, SimInfo]).


%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([Bot, SimInfo]) ->
    Conn = {conn,{slerl_sim_conn,start_link,[Bot, SimInfo]},
            permanent,2000,worker,[slerl_sim_conn]},

    Sim = {sim,{slerl_sim,start_link,[Bot, SimInfo]},
           permanent,2000,worker,[slerl_sim]},

    {ok,{{one_for_one,1,10}, [Conn, Sim]}}.


%%====================================================================
%% Internal functions
%%====================================================================
