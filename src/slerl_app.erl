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
-export([start/2, stop/1, prep_stop/1]).



%%====================================================================
%% API
%%====================================================================

launch() ->
    crypto:start(), % Just for exor :(
    application:start(inets),
    application:start(ssl),
    application:start(slerl),
    ok.


%%====================================================================
%% Application callbacks
%%====================================================================

start(_Type, StartArgs) ->
    ok = slerl_message:parse_message_template(),
    ok = slerl_uuid_db:startup(),
    case slerl_sup:start_link(StartArgs) of
        {ok, Pid} -> 
            {ok, Pid};
        Error ->
            Error
    end.

prep_stop(State) ->
    ?DBG(stopping),
    [B:logout() || {Bot,_,_,_} <- supervisor:which_children(slerl_sup),
                   B <- [{slerl_api, Bot}]],
    receive after 500 -> ok end,
    State.

stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
