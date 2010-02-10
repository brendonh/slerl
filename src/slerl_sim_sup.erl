%%%-------------------------------------------------------------------
%%% File    : slerl_sim_sup.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : Supervise simulator connections
%%%
%%% Created :  7 Feb 2010 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(slerl_sim_sup).

-behaviour(supervisor).

-include("slerl.hrl").
-include("slerl_util.hrl").

%% API
-export([start_link/1, start_sim_group/2]).

%% Supervisor callbacks
-export([init/1]).


%%====================================================================
%% API functions
%%====================================================================

start_link(Name) ->
    supervisor:start_link(?MODULE, [Name]).


start_sim_group(Name, SimInfo) ->
    ?DBG({procs, Name, ets:tab2list(Name)}),
    Self = ets:lookup_element(Name, sim_sup, 2),
    supervisor:start_child(Self, [SimInfo]),
    Sim = ets:lookup_element(Name, {sim, SimInfo#sim.ip, SimInfo#sim.port}, 2),
    gen_fsm:send_event(Sim, start_connect).


%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([Name]) ->
    ets:insert(Name, {sim_sup, self()}),
    ChildSpec = {none,{slerl_sim_group_sup,start_link,[Name]},
                 permanent,2000,supervisor,[slerl_sim_group_sup]},
    {ok,{{simple_one_for_one,1,10}, [ChildSpec]}}.


%%====================================================================
%% Internal functions
%%====================================================================
