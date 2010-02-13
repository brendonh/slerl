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
    Self = ets:lookup_element(Name, sim_sup, 2),
    Key = {sup, SimInfo#sim.ip, SimInfo#sim.port},
    ChildSpec = {Key,{slerl_sim_group_sup,start_link,[Name, SimInfo]},
                 transient,2000,supervisor,[slerl_sim_group_sup]},
    supervisor:start_child(Self, ChildSpec),
    Sim = ets:lookup_element(Name, {sim, SimInfo#sim.ip, SimInfo#sim.port}, 2),
    gen_server:cast(Sim, start_connect).


%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([Name]) ->
    ets:insert(Name, {sim_sup, self()}),
    {ok,{{one_for_one,1,10}, []}}.


%%====================================================================
%% Internal functions
%%====================================================================
