%%%-------------------------------------------------------------------
%%% File    : slerl_util.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : Utility functions
%%%
%%% Created :  6 Feb 2010 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(slerl_util).

-include("slerl_util.hrl").

-export([macaddr/0, macaddr/1, 
         md5_hex/1,
         parse_uuid/1, format_uuid/1,
         parse_ip/1,
         get_field/2,
         extract_string/1, get_string/2, get_binary_string/2,
         cross_product/2,
         convert_chat_code/1, convert_im_type/1
]).

macaddr() -> macaddr("eth0").

macaddr(IFName) ->
    {ok, [{hwaddr, MAC}]} = inet:ifget(IFName, [hwaddr]),
    string:join([lists:flatten(io_lib:format("~2.16.0B", [X]))
                 || X <- MAC], ":").


md5_hex(S) ->
       Md5_bin =  erlang:md5(S),
       Md5_list = binary_to_list(Md5_bin),
       lists:flatten(list_to_hex(Md5_list)).
 
list_to_hex(L) ->
       lists:map(fun(X) -> int_to_hex(X) end, L).
 
int_to_hex(N) when N < 256 ->
       [hex(N div 16), hex(N rem 16)].
 
hex(N) when N < 10 ->
       $0+N;
hex(N) when N >= 10, N < 16 ->
       $a + (N-10).


parse_uuid(S) ->
    I = erlang:list_to_integer([C || C <- S, C /= $-], 16),
    <<I:16/unsigned-integer-unit:8>>.

format_uuid(<<TL:32, TM:16, THV:16, CSR:8, CSL:8, N:48>>) -> 
    lists:flatten(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b", 
                                [TL, TM, THV, CSR, CSL, N])).
    

parse_ip(S) ->
    list_to_tuple(lists:map(fun list_to_integer/1, string:tokens(S, "."))).



get_field([], Message) -> Message;
get_field([K|Rest], Message) -> get_field(Rest, ?GV(K, Message)).

extract_string(Bin) ->
    {String, _} = split_binary(Bin, byte_size(Bin)-1),
    binary_to_list(String).

get_string(Key, Message) ->
    extract_string(get_field(Key, Message)).

get_binary_string(Key, Message) ->
    ZeroBin = get_field(Key, Message),
    {Bin, _} = split_binary(ZeroBin, byte_size(ZeroBin) - 1),
    Bin.


cross_product({AX, AY, AZ}, {BX, BY, BZ}) ->
    {(AY * AZ) - (BY * BZ),
     (AZ * BX) - (BZ * AZ),
     (AX * BY) - (BX * AY)}.



convert_chat_code(0) -> whisper;
convert_chat_code(1) -> say;
convert_chat_code(2) -> shout;
convert_chat_code(Other) -> {unknown, Other}.


convert_im_type(0) -> message_from_agent;
convert_im_type(1) -> message_box;
convert_im_type(3) -> group_invitation;
convert_im_type(4) -> inventory_offered;
convert_im_type(5) -> inventory_accepted;
convert_im_type(6) -> inventory_declined;
convert_im_type(7) -> group_vote;
convert_im_type(9) -> task_inventory_offered;
convert_im_type(10) -> task_inventory_accepted;
convert_im_type(11) -> task_inventory_declined;
convert_im_type(12) -> new_user_default;
convert_im_type(13) -> session_add;
convert_im_type(14) -> session_offline_add;
convert_im_type(15) -> session_group_start;
convert_im_type(16) -> session_cardless_start;
convert_im_type(17) -> session_send;
convert_im_type(18) -> session_drop;
convert_im_type(19) -> message_from_object;
convert_im_type(20) -> busy_auto_response;
convert_im_type(21) -> console_and_chat_history;
convert_im_type(22) -> request_teleport;
convert_im_type(23) -> accept_teleport;
convert_im_type(24) -> deny_teleport;
convert_im_type(25) -> god_like_request_teleport;
convert_im_type(26) -> currently_unused;
convert_im_type(28) -> goto_url;
convert_im_type(29) -> session911_start;
convert_im_type(30) -> lure911;
convert_im_type(31) -> from_task_as_alert;
convert_im_type(32) -> group_notice;
convert_im_type(33) -> group_notice_inventory_accepted;
convert_im_type(34) -> group_notice_inventory_declined;
convert_im_type(35) -> group_invitation_accept;
convert_im_type(36) -> group_invitation_decline;
convert_im_type(37) -> group_notice_requested;
convert_im_type(38) -> friendship_offered;
convert_im_type(39) -> friendship_accepted;
convert_im_type(40) -> friendship_declined;
convert_im_type(41) -> start_typing;
convert_im_type(42) -> stop_typing;
convert_im_type(Other) -> {unknown, Other}.
