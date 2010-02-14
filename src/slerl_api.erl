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
         im/2,
         retrieve_ims/0,
         subscribe/1, unsubscribe/1,
         get_region/1, get_uuids/1,
         trace/1, trace_filter/1,
        
         exact_avatar_uuid/1]).


%%====================================================================
%% Connection
%%====================================================================

connect() ->
    gen_server:cast(Bot, initial_connect).

logout() ->
    gen_server:cast(Bot, logout).


%%====================================================================
%% Control
%%====================================================================

position() ->
    gen_server:call(Bot, position).

block() ->
    gen_server:call(Bot, block).

teleport(Name, {_,_,_}=Pos) ->
    gen_server:call(Bot, {teleport, make_bin_string(Name), Pos}, 20000).


chat(Text) -> chat(normal, 0, Text).
chat(Type, Text) -> chat(Type, 0, Text).
chat(Type, Channel, Text) ->
    gen_server:call(Bot, {send_chat, 
                          slerl_codes:convert_chat_type(Type), 
                          Channel, 
                          make_bin_string(Text)}).

im(Name, Text) ->
    case exact_avatar_uuid(Name) of
        not_found -> {error, not_found};
        UUID -> gen_server:call(Bot, {send_im, UUID, make_bin_string(Text)})
    end.
            

retrieve_ims() ->
    gen_server:call(Bot, retrieve_ims).


%%====================================================================
%% Config / utility
%%====================================================================

get_region(Name) ->
    gen_server:call(Bot, {get_region, make_bin_string(Name)}).

get_uuids(AvatarName) ->
    gen_server:call(Bot, {find_avatar_uuids, make_bin_string(AvatarName)}).


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

make_bin_string(Str) when is_list(Str) -> make_bin_string(list_to_binary(Str));
make_bin_string(Bin) when is_binary(Bin) -> <<Bin/binary, 0>>.


%%====================================================================
%% Internal
%%====================================================================

exact_avatar_uuid(Name) when is_list(Name) ->
    exact_avatar_uuid(list_to_binary(Name));
exact_avatar_uuid(BinName) ->
    case slerl_uuid_db:get_uuid(BinName) of
        not_found ->
            case get_uuids(BinName) of
                {ok, Infos} ->
                    case find_uuid_by_name(BinName, Infos) of
                        not_found -> {error, not_found};
                        UUID -> UUID
                    end;
                Other -> {error, Other}
            end;
        UUID -> UUID
    end.
                            
find_uuid_by_name(_, []) -> not_found;
find_uuid_by_name(BinName, [{BinName, UUID}|_]) -> UUID;
find_uuid_by_name(BinName, [_|Rest]) -> find_uuid_by_name(BinName, Rest).
    
