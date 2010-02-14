%%%-------------------------------------------------------------------
%%% File    : slerl_api.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : Easy-to-use API
%%%
%%% Created : 11 Feb 2010 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(slerl_api, [Bot]).

-export([connect/0, logout/0, 
         position/0, block/0,
         teleport/2, 
         chat/1, chat/2, chat/3,
         retrieve_ims/0,
         subscribe/1, unsubscribe/1,
         get_region/1,
         trace/1, trace_filter/1]).


%%====================================================================
%% Connection
%%====================================================================

connect() ->
    gen_server:cast(Bot, initial_connect).

logout() ->
    slerl_bot:logout(Bot).


%%====================================================================
%% Control
%%====================================================================

position() ->
    gen_server:call(Bot, position).

block() ->
    gen_server:call(Bot, block).

teleport(Name, {_,_,_}=Pos) ->
    gen_server:call(Bot, {teleport, convert_name(Name), Pos}, 20000).


chat(Text) -> chat(normal, 0, Text).
chat(Type, Text) -> chat(Type, 0, Text).
chat(Type, Channel, Text) ->
    gen_server:call(Bot, {send_chat, 
                          slerl_codes:convert_chat_type(Type), 
                          Channel, 
                          make_bin_string(Text)}).

retrieve_ims() ->
    gen_server:call(Bot, retrieve_ims).


%%====================================================================
%% Config / utility
%%====================================================================

get_region(Name) ->
    gen_server:call(Bot, {get_region, convert_name(Name)}).


subscribe(Type) ->
    gen_server:cast(Bot, {subscribe, Type, self()}).

unsubscribe(Type) ->
    gen_server:cast(Bot, {unsubscribe, Type, self()}).


trace(Trace) -> gen_server:cast(Bot, {trace, Trace}).
    
trace_filter(MsgName) when is_atom(MsgName) ->
    trace_filter([MsgName]);
trace_filter(MsgNames) -> 
    gen_server:cast(Bot, {trace, {filter, MsgNames}}).


%%====================================================================
%% Conversions
%%====================================================================


convert_name(Name) when is_list(Name) -> convert_name(list_to_binary(Name));
convert_name(BinName) -> <<BinName/binary, 0>>.
    
make_bin_string(Str) when is_list(Str) -> make_bin_string(list_to_binary(Str));
make_bin_string(Bin) when is_binary(Bin) -> <<Bin/binary, 0>>.
