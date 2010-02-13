%%%-------------------------------------------------------------------
%%% File    : slerl_message.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : 
%%%
%%% Created :  7 Feb 2010 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(slerl_message).

-include("slerl.hrl").
-include("slerl_util.hrl").

-export([parse_message_template/0, build_message/2, parse_message/2, test/0]).
-export([zero_encode/1, zero_decode/1]).

parse_message_template() ->
    Content = slerl_message_template_lexer:scan_file("priv/message_template.msg"),
    {ok, {_Version, Messages}} = slerl_message_template_parser:parse(Content),

    Tid = ets:new(slerl_messages, [set, protected, named_table]),
    ets:insert(Tid, Messages),
    
    Tid2 = ets:new(slerl_messageByID, [set, protected, named_table]),
    ets:insert(Tid2, [{messageID(M), M}
                      || {_, M} <- Messages]),

    ok.

messageID(#messageDef{frequency=fixed}=M) -> {low, M#messageDef.number - 16#FFFF0000};
messageID(M) -> {M#messageDef.frequency, M#messageDef.number}.


%%-------------------------------------------------------------------
%% Message construction
%%-------------------------------------------------------------------

build_message(Name, Args) ->    
    Message = ets:lookup_element(slerl_messages, Name, 2),
    Blocks = build_blocks(Message#messageDef.blocks, Args),
    Bin = list_to_binary([Message#messageDef.messageID|Blocks]),
    {Final, Zeroed} = if Message#messageDef.zerocoded ->
                              CodedBin = zero_encode(Bin),
                              if byte_size(CodedBin) < byte_size(Bin) -> 
                                      {CodedBin, true};
                                 true -> 
                                      {Bin, false}
                              end;
                         true -> {Bin, false}
                      end,
    #message{spec=Message, message=Final, zerocoded=Zeroed}.
                

build_blocks(Blocks, Args) ->
    [build_block(B,A) || {B,A} <- lists:zip(Blocks, Args)].

build_block(#blockDef{quantity=single}=Block, Values) ->
    [build_parameter(P, V) || {{_,P},V} <- lists:zip(Block#blockDef.parameters, Values)];
build_block(#blockDef{quantity=multiple}=Block, ValueGroups) ->
    [build_parameter(P, V) || Values <- ValueGroups,
                              {{_,P},V} <- lists:zip(Block#blockDef.parameters, Values)];
build_block(#blockDef{quantity=variable}=Block, ValueGroups) ->
    Len = length(ValueGroups),
    [<<Len:1/unsigned-integer-unit:8>>
     |[build_parameter(P, V) || 
          Values <- ValueGroups,
          {{_,P},V} <- lists:zip(Block#blockDef.parameters, Values)]].



build_parameter(fixed, Val) -> Val;
build_parameter({variable, 1}, Val) -> 
    Size = byte_size(Val),
    <<Size:8/integer, Val/binary>>;
build_parameter({variable, 2}, Val) ->
    Size = byte_size(Val),
    <<Size:1/integer-little-unit:16, Val/binary>>;
build_parameter({uint, 1}, Val) -> <<Val:1/unsigned-integer-little-unit:8>>;
build_parameter({uint, 2}, Val) -> <<Val:2/unsigned-integer-little-unit:8>>;
build_parameter({uint, 4}, Val) -> <<Val:4/unsigned-integer-little-unit:8>>;
build_parameter({uint, 8}, Val) -> <<Val:8/unsigned-integer-little-unit:8>>;
build_parameter({int, 1}, Val) -> <<Val:1/signed-integer-little-unit:8>>;
build_parameter({int, 2}, Val) -> <<Val:2/signed-integer-little-unit:8>>;
build_parameter({int, 4}, Val) -> <<Val:4/signed-integer-little-unit:8>>;
build_parameter({int, 8}, Val) -> <<Val:8/signed-integer-little-unit:8>>;
build_parameter({float, 4}, Val) -> <<Val:1/float-little-unit:32>>;
build_parameter({float, 8}, Val) -> <<Val:1/float-little-unit:64>>;
build_parameter({vector3, 12}, {X,Y,Z}) -> 
    <<X:1/float-little-unit:32, Y:1/float-little-unit:32, Z:1/float-little-unit:32>>;
build_parameter({vector3, 24}, {X,Y,Z}) -> 
    <<X:1/float-little-unit:64, Y:1/float-little-unit:64, Z:1/float-little-unit:64>>;
build_parameter({vector4, 16}, {X,Y,Z,W}) -> 
    <<X:1/float-little-unit:32, Y:1/float-little-unit:32, Z:1/float-little-unit:32, W:1/float-little-unit:32>>;
build_parameter({quaternion, 12}, {X,Y,Z}) -> 
    <<X:1/float-little-unit:32, Y:1/float-little-unit:32, Z:1/float-little-unit:32>>;
build_parameter({uuid, 16}, Val) -> Val;
build_parameter({bool, 1}, true) -> <<1>>;
build_parameter({bool, 1}, false) -> <<0>>;
build_parameter({ipaddr, 4}, {N1,N2,N3,N4}) -> <<N1,N2,N3,N4>>;
build_parameter({ipport, 2}, Val) -> <<Val:2/unsigned-integer-little-unit:8>>.



%%-------------------------------------------------------------------
%% Message parsing
%%-------------------------------------------------------------------

parse_message(<<255, 255, LowID:1/integer-unit:16, Rest/binary>>, Message) ->
    parse_message2({low, LowID}, Rest, Message);
parse_message(<<255, MedID:1/integer-unit:8, Rest/binary>>, Message) ->
    parse_message2({medium, MedID}, Rest, Message);
parse_message(<<HighID:1/integer-unit:8, Rest/binary>>, Message) ->
    parse_message2({high, HighID}, Rest, Message).
    
parse_message2(MID, Rest, Message) ->
    Spec = ets:lookup_element(slerl_messageByID, MID, 2),

    Decoded = if Message#message.zerocoded -> zero_decode(Rest);
                 true -> Rest end,
    {Blocks, Tail} = parse_blocks(Spec#messageDef.blocks, Decoded, []),

    case {Tail, Spec#messageDef.name} of
        {_, 'TestMessage'} -> ok;
        {<<>>, _} -> ok;
        _ -> ?DBG({tail, Spec#messageDef.name, Tail})
    end,

    Message#message{spec=Spec, message=Blocks}.


parse_blocks([], Tail, Buff) ->
    {lists:reverse(Buff), Tail};
parse_blocks([Block|Blocks], Bin, Buff) ->
    {Fields, Tail} = parse_block(Block, Bin),
    parse_blocks(Blocks, Tail, [Fields|Buff]).


parse_block(#blockDef{quantity=single}=B, Bin) ->
    {Parameters, Tail} = parse_parameters(B#blockDef.parameters, Bin, []),
    {{B#blockDef.name, Parameters}, Tail};
parse_block(#blockDef{quantity=multiple, repeats=Repeats}=B, Bin) ->
    {Parameters, Tail} = parse_multiple(B#blockDef.parameters, Bin, [], Repeats),
    {{B#blockDef.name, Parameters}, Tail};
parse_block(#blockDef{quantity=variable}=B, <<Repeats:1/integer-unit:8, Bin/binary>>) ->
    {Parameters, Tail} = parse_multiple(B#blockDef.parameters, Bin, [], Repeats),
    {{B#blockDef.name, Parameters}, Tail}.


parse_multiple(_, Bin, Buff, 0) -> 
    {lists:reverse(Buff), Bin};
parse_multiple(PS, Bin, Buff, Repeats) ->
    {P, Tail} = parse_parameters(PS, Bin, []),
    parse_multiple(PS, Tail, [P|Buff], Repeats-1).


parse_parameters([], Tail, Buff) ->
    {lists:reverse(Buff), Tail};
parse_parameters([{Name,Type}|Rest], Bin, Buff) ->
    {Val, Tail} = parse_parameter(Type, Bin),
    parse_parameters(Rest, Tail, [{Name, Val}|Buff]).


parse_parameter({fixed, Len}, Bin) -> 
    <<Val:Len/binary, Rest/binary>> = Bin,
    {Val, Rest};
parse_parameter({variable, 1}, <<Len:1/integer-unit:8, Bin/binary>>) -> 
    <<Val:Len/binary, Rest/binary>> = Bin,
    {Val, Rest};
parse_parameter({variable, 2}, <<Len:1/integer-little-unit:16, Bin/binary>>) -> 
    <<Val:Len/binary, Rest/binary>> = Bin,
    {Val, Rest};
parse_parameter({uint, 1}, <<Val:1/unsigned-integer-little-unit:8, Rest/binary>>) -> {Val, Rest};
parse_parameter({uint, 2}, <<Val:2/unsigned-integer-little-unit:8, Rest/binary>>) -> {Val, Rest};
parse_parameter({uint, 4}, <<Val:4/unsigned-integer-little-unit:8, Rest/binary>>) -> {Val, Rest};
parse_parameter({uint, 8}, <<Val:8/unsigned-integer-little-unit:8, Rest/binary>>) -> {Val, Rest};
parse_parameter({int, 1}, <<Val:1/signed-integer-little-unit:8, Rest/binary>>) -> {Val, Rest};
parse_parameter({int, 2}, <<Val:2/signed-integer-little-unit:8, Rest/binary>>) -> {Val, Rest};
parse_parameter({int, 4}, <<Val:4/signed-integer-little-unit:8, Rest/binary>>) -> {Val, Rest};
parse_parameter({int, 8}, <<Val:8/signed-integer-little-unit:8, Rest/binary>>) -> {Val, Rest};
parse_parameter({float, 4}, <<Val:1/float-little-unit:32, Rest/binary>>) -> {Val, Rest};
parse_parameter({float, 8}, <<Val:1/float-little-unit:64, Rest/binary>>) -> {Val, Rest};
parse_parameter(
  {vector3, 12}, 
  <<X:1/float-little-unit:32, Y:1/float-little-unit:32, Z:1/float-little-unit:32, Rest/binary>>) ->
    {{X,Y,Z}, Rest};
parse_parameter(
  {vector3, 24},
  <<X:1/float-little-unit:64, Y:1/float-little-unit:64, Z:1/float-little-unit:64, Rest/binary>>) ->
    {{X,Y,Z}, Rest};
parse_parameter(
  {vector4, 16},
  <<X:1/float-little-unit:32, Y:1/float-little-unit:32, 
    Z:1/float-little-unit:32, W:1/float-little-unit:32, Rest/binary>>) ->
    {{X,Y,Z,W}, Rest};
parse_parameter(
  {quaternion, 12},
  <<X:1/float-little-unit:32, Y:1/float-little-unit:32, Z:1/float-little-unit:32, Rest/binary>>) ->
    {{X,Y,Z}, Rest};
parse_parameter({uuid, 16}, <<Val:16/binary, Rest/binary>>) -> {Val, Rest};
parse_parameter({bool, 1}, <<1, Rest/binary>>) -> {true, Rest};
parse_parameter({bool, 1}, <<0, Rest/binary>>) -> {false, Rest};
parse_parameter({ipaddr, 4}, <<N1,N2,N3,N4, Rest/binary>>) -> {{N1,N2,N3,N4}, Rest};
parse_parameter({ipport, 2}, <<Val:2/unsigned-integer-little-unit:8, Rest/binary>>) -> {Val, Rest}.
    


%%-------------------------------------------------------------------
%% Utilities
%%-------------------------------------------------------------------

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
    parse_message_template(),
    ?DBG(build_message('OpenCircuit', [ [{192,168,1,5}, 9743] ])),
    ?DBG(build_message('TestMessage', [ [4324],
                                        [[1, 2, 3],
                                         [4, 5, 6],
                                         [7,8,9]]
                                       ])),
    ?DBG(build_message('PacketAck', [ [[1],[2],[3],[454535]] ])),
    ?DBG(build_message('UseCircuitCode', 
                       [ [987321,
                          slerl_util:parse_uuid("fc1ae5e3-debb-46e9-8636-b71377fcf853"),
                          slerl_util:parse_uuid("e00cd630-a033-4eab-a466-7aeeecc1b00c")] ])).
