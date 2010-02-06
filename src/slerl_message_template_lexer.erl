%%%-------------------------------------------------------------------
%%% File    : slerl_message_lexer.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : Lex SL message template
%%%
%%% Created :  7 Feb 2010 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(slerl_message_template_lexer).

-export([scan/1, scan_file/1]).


scan_file(Filename) ->
    {ok, Content} = file:read_file(Filename),
    scan(binary_to_list(Content)).


scan(Content) ->
    scan(Content, 1, []).

scan("//" ++ Rest, L, B) -> scan(skip_line(Rest), L+1, B);
scan([$\n|Rest], L, B) -> scan(Rest, L+1, B);
scan([$\r|Rest], L, B) -> scan(Rest, L, B);
scan([$\s|Rest], L, B) -> scan(Rest, L, B);
scan([$\t|Rest], L, B) -> scan(Rest, L, B);
scan([${|Rest], L, B) -> scan(Rest, L, [{'{',L}|B]);
scan([$}|Rest], L, B) -> scan(Rest, L, [{'}',L}|B]);
scan([], _, B) -> lists:reverse(B);
scan(More, L, B) -> 
    {Word, Rest} = get_word(More, []),
    scan(Rest, L, [{'word', L, lists:reverse(Word)}|B]).

skip_line([$\n|Rest]) -> Rest;
skip_line([_|Rest]) -> skip_line(Rest).

get_word([$\n|_]=Rest, B) -> {B, Rest};
get_word([$\r|_]=Rest, B) -> {B, Rest};
get_word([$\s|_]=Rest, B) -> {B, Rest};
get_word([$\t|_]=Rest, B) -> {B, Rest};
get_word([C|Rest],B) -> get_word(Rest, [C|B]).
