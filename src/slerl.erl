%%%-------------------------------------------------------------------
%%% File    : slerl.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : High-level API
%%%
%%% Created :  7 Feb 2010 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(slerl).

-include("slerl.hrl").
-include("slerl_util.hrl").

-export([login_loop/4, login/3, login/4]).

-define(FIRST_RETRY_INTERVAL, 2000).


login_loop(First, Last, Password, Max) ->
    login_loop(First, Last, Password, Max, -1, ?FIRST_RETRY_INTERVAL).

login_loop(_, _, _, Max, Max, _) ->
    {error, max_retry_exceeded};
login_loop(First, Last, Password, Max, Count, Delay) ->
    case login(First, Last, Password) of
        {ok, Name} -> {ok, Name};
        {error, {login_failed, presence}} ->
            ?DBG({retrying_in, Delay}),
            timer:sleep(Delay), 
            login_loop(First, Last, Password, Max, Count+1, Delay*2);
        Other ->
            Other
    end.
            


login(First, Last, Password) -> login(First, Last, Password, "last").
     
login(First, Last, Password, Start) ->
    ?DBG({logging_in, First, Last}),
    {ok, URL} = application:get_env(slerl, login_url),
    {ok, VersionInts} = application:get_env(slerl, client_version),
    Version = list_to_tuple([integer_to_list(I) || I <- tuple_to_list(VersionInts)]),
    Response = slerl_login:login(URL, First, Last, Password, Version, Start),
    case Response of
        {ok, Info} -> 
            Name = list_to_atom(lists:flatten([First, $\s, Last])),
            ?DBG({xmlrpc_login_succeeded, Name}),
            start_bot(Name, Info);
        Other -> 
            Other
    end.


start_bot(Name, Info) ->   
    slerl_sup:start_bot(Name, Info),
    gen_server:cast(Name, initial_connect),
    {ok, Name}.


