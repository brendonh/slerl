%%%-------------------------------------------------------------------
%%% File    : slerl_api.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : Easy-to-use API
%%%
%%% Created : 11 Feb 2010 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(slerl_api, [Bot]).

-export([logout/0, get_region/1, trace/1]).


logout() ->
    slerl_bot:logout(Bot).


get_region(Name) when is_list(Name) ->
    get_region(list_to_binary(Name));
get_region(Name) when is_binary(Name) ->
    gen_server:call(Bot, {get_region, <<Name/binary, 0>>}).


trace(Trace) -> gen_server:cast(Bot, {trace, Trace}).
    
