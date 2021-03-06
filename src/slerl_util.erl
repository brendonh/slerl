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
         parse_uuid/1, format_uuid/1, random_uuid/0,
         parse_ip/1,
         strip_zero/1, extract_string/1, 
         get_field/2, get_string/2, get_binary_string/2,
         cross_product/2
]).


%%====================================================================
%% Login-related
%%====================================================================

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



%%====================================================================
%% UUIDs
%%====================================================================

parse_uuid(S) ->
    I = erlang:list_to_integer([C || C <- S, C /= $-], 16),
    <<I:16/unsigned-integer-unit:8>>.

format_uuid(<<TL:32, TM:16, THV:16, CSR:8, CSL:8, N:48>>) -> 
    lists:flatten(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b", 
                                [TL, TM, THV, CSR, CSL, N])).
    
random_uuid() -> 
    {A1,A2,A3} = erlang:now(),
    random:seed(A1, A2, A3),
    <<(random:uniform(4294967296) - 1):32,
     (random:uniform(4294967296) - 1):32,
     (random:uniform(4294967296) - 1):32,
     (random:uniform(4294967296) - 1):32>>.


parse_ip(S) ->
    list_to_tuple(lists:map(fun list_to_integer/1, string:tokens(S, "."))).



%%====================================================================
%% Message field extraction
%%====================================================================

get_field([], Message) -> Message;
get_field([K|Rest], Message) -> get_field(Rest, ?GV(K, Message)).

strip_zero(ZeroBin) ->
    {Bin, _} = split_binary(ZeroBin, byte_size(ZeroBin)-1),
    Bin.

extract_string(Bin) ->
    binary_to_list(strip_zero(Bin)).

get_string(Key, Message) ->
    extract_string(get_field(Key, Message)).

get_binary_string(Key, Message) ->
    ZeroBin = get_field(Key, Message),
    strip_zero(ZeroBin).


%%====================================================================
%% 3D Math
%%====================================================================

cross_product({AX, AY, AZ}, {BX, BY, BZ}) ->
    {(AY * AZ) - (BY * BZ),
     (AZ * BX) - (BZ * AZ),
     (AX * BY) - (BX * AY)}.


