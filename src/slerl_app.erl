%%%-------------------------------------------------------------------
%%% File    : slerl_app.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : 
%%%
%%% Created :  4 Feb 2010 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(slerl_app).

-behaviour(application).

-include("slerl.hrl").
-include("slerl_util.hrl").


%% API
-export([launch/0]).

%% Application callbacks
-export([start/2, stop/1]).



%%====================================================================
%% API
%%====================================================================

launch() ->
    application:start(inets),
    application:start(ssl),
    application:start(slerl),
    {ok, [Bits]} = file:consult("login.config"),
    First = ?GV(first, Bits),
    Last = ?GV(last, Bits),
    Password = ?GV(pass, Bits),
    case slerl:login(First, Last, Password) of
        {ok, _Name} -> 
            ok;
        Other ->
            ?DBG({oh_noes, Other}),
            init:stop()
    end.



%%====================================================================
%% Application callbacks
%%====================================================================

start(_Type, StartArgs) ->
    ok = slerl_message:parse_message_template(),
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
