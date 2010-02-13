%%%-------------------------------------------------------------------
%%% File    : slerl_api.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : Easy-to-use API
%%%
%%% Created : 11 Feb 2010 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(slerl_api, [Bot]).

-export([logout/0, 
         position/0, block/0,
         teleport/2, 
         get_region/1,
         trace/1, trace_filter/1]).


logout() ->
    slerl_bot:logout(Bot).


position() ->
    gen_server:call(Bot, position).

block() ->
    gen_server:call(Bot, block).

teleport(Name, {_,_,_}=Pos) ->
    gen_server:call(Bot, {teleport, convert_name(Name), Pos}, 20000).


get_region(Name) ->
    gen_server:call(Bot, {get_region, convert_name(Name)}).


trace(Trace) -> gen_server:cast(Bot, {trace, Trace}).
    
trace_filter(MsgName) when is_atom(MsgName) ->
    trace_filter([MsgName]);
trace_filter(MsgNames) -> 
    gen_server:cast(Bot, {trace, {filter, MsgNames}}).

convert_name(Name) when is_list(Name) -> convert_name(list_to_binary(Name));
convert_name(BinName) -> <<BinName/binary, 0>>.
    
