%%%-------------------------------------------------------------------
%%% File    : slerl_sup.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : 
%%%
%%% Created :  4 Feb 2010 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(slerl_sup).

-behaviour(supervisor).

-include("slerl.hrl").
-include("slerl_util.hrl").

%% API
-export([start_link/1, start_bot/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(StartArgs) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, StartArgs).

start_bot(Name, Info) ->
     supervisor:start_child(?SERVER, [Name, Info]),
     {ok, whereis(Name)}.


%%====================================================================
%% Supervisor callbacks
%%====================================================================

init(_StartArgs) ->
    ChildSpec = {none, {slerl_bot_sup, start_link, []},
                 transient,20000,supervisor,[slerl_bot_sup]},

    {ok,{{simple_one_for_one,10,60}, [ChildSpec]}}.


%%====================================================================
%% Internal functions
%%====================================================================

