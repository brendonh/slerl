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

-export([login/3, login/4, logout/1]).


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
        Other -> Other
    end.



start_bot(Name, Info) ->   
    slerl_sup:start_bot(Name, Info),
    gen_server:cast(Name, initial_connect),
    {ok, Name}.

logout(Bot) ->
    slerl_bot:logout(Bot).
