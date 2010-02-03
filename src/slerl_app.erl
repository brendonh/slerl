%%%-------------------------------------------------------------------
%%% File    : slerl_app.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : 
%%%
%%% Created :  4 Feb 2010 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(slerl_app).

-behaviour(application).

%% API
-export([launch/0]).

%% Application callbacks
-export([start/2, stop/1]).


%%====================================================================
%% API
%%====================================================================

launch() ->
    application:start(slerl).


%%====================================================================
%% Application callbacks
%%====================================================================

start(_Type, StartArgs) ->
    case slerl_sup:start_link(StartArgs) of
        {ok, Pid} -> 
            {ok, Pid};
        Error ->
            Error
    end.

stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
