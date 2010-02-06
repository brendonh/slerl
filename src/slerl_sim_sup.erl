%%%-------------------------------------------------------------------
%%% File    : slerl_sim_sup.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : Supervise simulator connections
%%%
%%% Created :  7 Feb 2010 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(slerl_sim_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).


%%====================================================================
%% API functions
%%====================================================================

start_link(Info) ->
    supervisor:start_link(?MODULE, [Info]).


%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([Info]) ->
    ChildSpec = {none,{slerl_sim,start_link,[Info]},
                 permanent,2000,worker,[slerl_sim]},
    {ok,{{simple_one_for_one,1,10}, [ChildSpec]}}.


%%====================================================================
%% Internal functions
%%====================================================================
