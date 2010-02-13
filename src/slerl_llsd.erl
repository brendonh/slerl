%%%-------------------------------------------------------------------
%%% File    : slerl_llsd.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : LLSD encoding and decoding
%%%
%%% Created : 11 Feb 2010 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(slerl_llsd).

-include_lib("xmerl/include/xmerl.hrl").

-export([encode_xml/1, decode_xml/1]).
-export([test/0]).

-define(D(T), io:format("~p~n", [T])).

%%--------------------------------------------------------------------
%%% Encoding
%%--------------------------------------------------------------------

encode_xml(Stuff) ->
    Content = [{llsd, [encode(Stuff)]}],
    XML = xmerl:export_simple(Content,xmerl_xml),
    lists:flatten(XML).

encode(undefined) -> undef;
encode(null) -> undef;
encode(true) -> {boolean, ["1"]};
encode(false) -> boolean;
encode(A) when is_atom(A) -> A;
encode(X) when is_integer(X) -> {integer, [integer_to_list(X)]};
encode(X) when is_float(X) -> {real, [float_to_list(X)]};
encode({uuid, UUID}) when is_list(UUID) -> {uuid, [UUID]};
encode({uuid, UUID}) when is_binary(UUID) -> {uuid, [slerl_util:format_uuid(UUID)]};
encode({string, Str}) when is_list(Str) -> {string, [Str]};
encode({string, Bin}) when is_binary(Bin) -> {string, [binary_to_list(Bin)]};
encode({binary, Bin}) when is_binary(Bin) -> {binary, [base64:encode_to_string(Bin)]};
encode({date, Date}) when is_list(Date) -> {date, [Date]};
encode({date, {{_,_,_},{_,_,_}}=DT}) -> {date, [iso_8601_fmt(DT)]};
encode({uri, Str}) -> {uri, [Str]};
encode({map, List}) -> {map, lists:flatten([[{key,[encode_key(K)]},encode(V)] || {K,V} <- List])};
encode(List) when is_list(List) -> {array, lists:map(fun encode/1, List)}.

encode_key(K) when is_atom(K) -> atom_to_list(K);
encode_key(B) when is_binary(B) -> binary_to_list(B);
encode_key(S) when is_list(S) -> S.

    
iso_8601_fmt(DateTime) ->
    {{Year,Month,Day},{Hour,Min,Sec}} = DateTime,
    lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B:00Z",
        [Year, Month, Day, Hour, Min, Sec])).


%%--------------------------------------------------------------------
%%% Decoding
%%--------------------------------------------------------------------

decode_xml(Str)->
    {#xmlElement{name=llsd, content=Content}, Tail} = xmerl_scan:string(Str),

    case Tail of
        [] -> ok;
        _ -> ?D({xml_tail, Tail})
    end,

    [Stuff] = lists:map(fun decode/1, Content),
    Stuff.


decode(#xmlElement{name=undef}) -> null;
decode(#xmlElement{name=boolean, content=[]}) -> false;
decode(#xmlElement{name=boolean}) -> true;
decode(#xmlElement{name=string, content=[]}) -> ""; % Hrm
decode(#xmlElement{name=string, content=[Str|_]}) -> get_text(Str);
decode(#xmlElement{name=map, content=KV}) -> decode_map(KV, []);
decode(#xmlElement{name=array, content=L}) -> lists:map(fun decode/1, L);
decode(#xmlElement{content=[]}) -> null;
decode(#xmlElement{name=integer, content=[X|_]}) -> list_to_integer(get_text(X));
decode(#xmlElement{name=real, content=[X|_]}) -> float_please(get_text(X));
decode(#xmlElement{name=uuid, content=[UUID|_]}) -> get_text(UUID);
decode(#xmlElement{name=binary, content=[B64|_]}) -> base64:decode_to_string(get_text(B64));
decode(#xmlElement{name=date, content=[DateStr|_]}) -> iso_8601_decode(get_text(DateStr));
decode(#xmlElement{name=uri, content=[URI|_]}) -> get_text(URI).

iso_8601_decode(Str) ->
    [Year,Month,Day,Hour,Min,Sec|_] = string:tokens(Str, "-T:Z"),
    {{Year,Month,Day},{Hour,Min,Sec}}.

get_text(#xmlText{value=T}) -> T;
get_text(#xmlElement{content=[#xmlText{value=T}]}) -> T.

decode_map([], Buff) -> lists:reverse(Buff);
decode_map([K,V|Rest], Buff) -> 
    decode_map(Rest, [{list_to_atom(get_text(K)),decode(V)}|Buff]).
     

float_please(S) ->
    case lists:member($., S) of
        true -> list_to_float(S);
        _ -> float(list_to_integer(S))
    end.
            

%%--------------------------------------------------------------------


test() -> 
    XML = encode_xml([null, true, 15, 3.14, {uuid, "e00cd630-a033-4eab-a466-7aeeecc1b00c"},
                      {uuid, <<111,40,190,84,171,201,64,80,134,194,85,247,140,30,77,228>>},
                      {string, "Hello"}, {string, <<"World">>}, {binary, <<"Haha">>},
                      {date, calendar:local_time_to_universal_time(erlang:localtime())},
                      {date, "2006-02-01T14:29:53.43Z"},  {uri, "http://sim956.agni.lindenlab.com:12035/runtime/agents"},
                      {map, [{k1, {string, "v1"}}, {k2, 19}]} 
                     ]),
    ?D(decode_xml(XML)).
               
    
