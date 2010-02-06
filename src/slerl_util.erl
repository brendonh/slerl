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
         parse_message_template/0,
         zero_encode/1, zero_decode/1
         ]).


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


parse_message_template() ->
    Content = slerl_message_template_lexer:scan_file("priv/message_template.msg"),
    {ok, {_Version, Messages}} = slerl_message_template_parser:parse(Content),
    ?DBG(Messages).



zero_encode(B) ->
    zero_encode(B, []).

zero_encode(<<>>, Buff) ->
    list_to_binary(lists:reverse(Buff));
zero_encode(<<0:8/integer, Rest/binary>>, Buff) ->
    {Run, Rest2} = run_zeros(Rest, 1),
    zero_encode(Rest2, [Run|Buff]);
zero_encode(<<I:8/integer, Rest/binary>>, Buff) ->
    zero_encode(Rest, [I|Buff]).


run_zeros(Rest, Count) when Count == 255 ->
    {<<0:8/integer, Count:8/integer>>, Rest};
run_zeros(<<0:8/integer, Rest/binary>>, Count) -> 
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


    
