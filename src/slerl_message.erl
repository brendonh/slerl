%%%-------------------------------------------------------------------
%%% File    : slerl_message.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : 
%%%
%%% Created :  7 Feb 2010 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(slerl_message).

-export([parse_message_template/0]).

parse_message_template() ->
    Content = slerl_message_template_lexer:scan_file("priv/message_template.msg"),
    {ok, {_Version, Messages}} = slerl_message_template_parser:parse(Content),
    Tid = ets:new(slerl_messages, [set, protected, named_table]),
    ets:insert(Tid, Messages),
    {ok, Tid}.
