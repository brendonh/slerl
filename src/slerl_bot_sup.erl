%%%-------------------------------------------------------------------
%%% File    : slerl_bot_sup.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : 
%%%
%%% Created :  7 Feb 2010 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(slerl_bot_sup).

-include("slerl_util.hrl").

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(Info) ->
    ?DBG({?MODULE, starting}),
    supervisor:start_link(?MODULE, [Info]).


%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([Info]) ->
    Sims = {udp, {slerl_sim_sup, start_link, [Info]},
            permanent,2000,supervisor,[slerl_sim_sup]},
    {ok,{{one_for_one,0,1}, [Sims]}}.


%%====================================================================
%% Internal functions
%%====================================================================
