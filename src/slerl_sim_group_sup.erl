%%%-------------------------------------------------------------------
%%% File    : slerl_sim_group_sup.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : Collection of processes for a single sim connection
%%%
%%% Created :  7 Feb 2010 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(slerl_sim_group_sup).

-behaviour(supervisor).

-include("slerl.hrl").

%% API
-export([start_link/2, start_event_queue/2]).

%% Supervisor callbacks
-export([init/1]).


%%====================================================================
%% API functions
%%====================================================================

start_link(Name, SimInfo) ->
    supervisor:start_link(?MODULE, [Name, SimInfo]).


start_event_queue(Name, SimInfo) ->
    Self = ets:lookup_element(Name, {sup, SimInfo#sim.ip, SimInfo#sim.port}, 2),
    supervisor:start_child(
      Self,
      {Name, {slerl_event_queue, start_link, [Name, SimInfo]},
       transient,5000,worker,[slerl_event_queue]}).


%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([Name, SimInfo]) ->

    ets:insert(Name, {{sup, SimInfo#sim.ip, SimInfo#sim.port}, self()}),

    Conn = {conn,{slerl_sim_conn,start_link,[Name, SimInfo]},
            permanent,2000,worker,[slerl_sim_conn]},

    Sim = {sim,{slerl_sim,start_link,[Name, SimInfo]},
           permanent,2000,worker,[slerl_sim]},

    {ok,{{one_for_one,1,10}, [Conn, Sim]}}.


%%====================================================================
%% Internal functions
%%====================================================================
