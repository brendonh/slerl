%%%-------------------------------------------------------------------
%%% File    : slerl_bot_sup.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : 
%%%
%%% Created :  7 Feb 2010 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(slerl_bot_sup).

-include("slerl.hrl").
-include("slerl_util.hrl").

-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(Name, Info) ->
    supervisor:start_link(?MODULE, [Name, Info]).
    

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([Name, Info]) ->
    process_flag(trap_exit, true),
    Bot = {bot, {slerl_bot, start_link, [Name, Info, self()]},
               permanent,2000,worker,[slerl_bot]},
    Sims = {sims, {slerl_sim_sup, start_link, [Name]},
            permanent,2000,supervisor,[slerl_sim_sup]},
    {ok,{{one_for_one,0,1}, [Bot, Sims]}}.


%%====================================================================
%% Internal functions
%%====================================================================
