%%%-------------------------------------------------------------------
%%% File    : slerl_util.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : Utility functions
%%%
%%% Created :  6 Feb 2010 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(slerl_util).

-export([macaddr/0, macaddr/1, 
         md5_hex/1, 
         zero_encode/1, zero_decode/1,
         test/0]).


-define(DBG(T), io:format("~p ~p~n", [self(), T])).


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


zero_encode(B) ->
    zero_encode(B, []).

zero_encode(<<>>, Buff) ->
    list_to_binary(lists:reverse(Buff));
zero_encode(<<0:8/integer, Rest/binary>>, Buff) ->
    {Run, Rest2} = run_zeros(Rest, 1),
    zero_encode(Rest2, [Run|Buff]);
zero_encode(<<I:8/integer, Rest/binary>>, Buff) ->
    zero_encode(Rest, [I|Buff]).


run_zeros(<<0:8/integer, Rest/binary>>, Count) when Count < 255 -> 
    run_zeros(Rest, Count+1);
run_zeros(Rest, Count) -> 
    {<<0:8/integer, Count:8/integer>>, Rest}.


zero_decode(B) ->
    zero_decode(B, []).

zero_decode(<<>>, Buff) ->
    list_to_binary(lists:reverse(Buff));
zero_decode(<<0:8/integer, Count:8/integer, Rest/binary>>, Buff) ->
    Bits = Count * 8,
    zero_decode(Rest, [<<0:Bits/integer>>|Buff]);
zero_decode(<<I:8/integer, Rest/binary>>, Buff) ->
    zero_decode(Rest, [I|Buff]).


test() ->

    A = <<1, 5, 0, 0, 0, 0, 23, 0, 0, 0, 9>>,
    B = list_to_binary([<<1, 3>>,
                        <<0:2048/integer>>,
                        <<0, 0, 9, 12>>]),

    One = zero_encode(A),
    Two = zero_encode(B),
    ?DBG(One),
    ?DBG(Two),
    ?DBG(zero_decode(One)),
    ?DBG(zero_decode(Two)),
    
    ?DBG(zero_decode(One) == A),
    ?DBG(zero_decode(Two) == B).
    
