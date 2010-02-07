Nonterminals template version
             messages message messagename frequency number trustlevel zerocoded messageflag
             blocks block blockname quantity repeats
             parameters parameter parametername type prefixlength.

Terminals 'word' '{' '}'.

Rootsymbol template.

template -> version messages : {'$1', '$2'}.

version -> 'word' 'word': {'version', value_of('$2')}.

messages -> message: ['$1'].
messages -> message messages: ['$1'|'$2'].


message -> '{' messagename frequency number trustlevel zerocoded messageflag blocks '}' :
        assemble_message('$2', '$3', '$4', '$5', '$6', '$7', '$8').

message -> '{' messagename frequency number trustlevel zerocoded blocks '}' :
        assemble_message('$2', '$3', '$4', '$5', '$6', none, '$7').

message -> '{' messagename frequency number trustlevel zerocoded '}' :
        assemble_message('$2', '$3', '$4', '$5', '$6', none, []).


messagename -> 'word': value_of('$1').
frequency -> 'word': parse_frequency(value_of('$1')).
number -> 'word': parse_number(value_of('$1')).
trustlevel -> 'word': parse_trustlevel(value_of('$1')).
zerocoded -> 'word': parse_zerocoded(value_of('$1')).
messageflag -> 'word': parse_messageflag(value_of('$1')).

blocks -> block: ['$1'].
blocks -> block blocks: ['$1'|'$2'].

block -> '{' blockname quantity parameters '}' : 
      #blockDef{name='$2', quantity='$3', repeats=1, parameters='$4'}.
block -> '{' blockname quantity repeats parameters '}' : 
      #blockDef{name='$2', quantity='$3', repeats='$4', parameters='$5'}.

blockname -> 'word': value_of('$1').
quantity -> 'word': parse_quantity(value_of('$1')).
repeats -> 'word': list_to_integer(value_of('$1')).

parameters -> parameter: ['$1'].
parameters -> parameter parameters: ['$1'|'$2'].

parameter ->  '{' parametername type '}': {'$2', '$3'}.
parameter ->  '{' parametername type prefixlength '}': {'$2', {'$3', '$4'}}.

parametername -> 'word': value_of('$1').
type -> 'word': parse_type(value_of('$1')).
prefixlength -> 'word': list_to_integer(value_of('$1')).

Erlang code.

-include("include/slerl.hrl").

value_of(T) -> element(3, T).

assemble_message(Name, Freq, Num, Trust, Zero, Flag, Blocks) ->
   {list_to_atom(Name), 
    #messageDef{name=Name, 
                frequency=Freq, 
                number=Num, 
                messageID=precalc_messageID(Freq, Num),
                trusted=Trust, 
                zerocoded=Zero, 
                flag=Flag, 
                blocks=Blocks}}.


parse_frequency("Fixed") -> fixed;
parse_frequency("Low") -> low;
parse_frequency("Medium") -> medium;
parse_frequency("High") -> high.

parse_number("0x" ++ Hex) -> erlang:list_to_integer(Hex, 16);
parse_number(Dec) -> list_to_integer(Dec).

parse_trustlevel("NotTrusted") -> false;
parse_trustlevel("Trusted") -> true.

parse_zerocoded("Zerocoded") -> true;
parse_zerocoded("Unencoded") -> false.

parse_messageflag("Deprecated") -> deprecated;
parse_messageflag("UDPDeprecated") -> udpdeprecated;
parse_messageflag("UDPBlackListed") -> blacklisted.

parse_quantity("Single") -> single;
parse_quantity("Variable") -> variable;
parse_quantity("Fixed") -> multiple;
parse_quantity("Multiple") -> multiple.

parse_type("Null") -> null;
parse_type("Fixed") -> fixed;
parse_type("Variable") -> variable;
parse_type("U8") -> {uint, 1};
parse_type("U16") -> {uint, 2};
parse_type("U32") -> {uint, 4};
parse_type("U64") -> {uint, 8};
parse_type("S8") -> {int, 1};
parse_type("S16") -> {int, 2};
parse_type("S32") -> {int, 4};
parse_type("S64") -> {int, 8};
parse_type("F32") -> {float, 4};
parse_type("F64") -> {float, 8};
parse_type("LLVector3") -> {vector3, 12};
parse_type("LLVector3d") -> {vector3, 24};
parse_type("LLVector4") -> {vector4, 16};
parse_type("LLQuaternion") -> {quaternion, 12};
parse_type("LLUUID") -> {uuid, 16};
parse_type("BOOL") -> {bool, 1};
parse_type("IPADDR") -> {ipaddr, 4};
parse_type("IPPORT") -> {ipport, 2};
parse_type("U16Vec3") -> not_used;
parse_type("U16Quat") -> not_used;
parse_type("S16Array") -> not_used.

precalc_messageID(high, Number) -> <<Number:1/integer-unit:8>>;
precalc_messageID(fixed, Number) -> <<Number:1/integer-unit:8>>;
precalc_messageID(medium, Number) -> <<255:1/integer-unit:8, Number:1/integer-unit:8>>;
precalc_messageID(low, Number) -> <<255:1/integer-unit:8, 255:1/integer-unit:8, Number:1/integer-unit:16>>.
