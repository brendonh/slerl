-module(slerl_sandbox).

-include("slerl.hrl").
-include("slerl_util.hrl").

-export([test/0]).

test() ->
    application:start(slerl),
    {ok, [Bits]} = file:consult("login.config"),
    First = ?GV(first, Bits),
    Last = ?GV(last, Bits),
    Password = ?GV(pass, Bits),
    case slerl:login_loop(First, Last, Password, 5) of
        {ok, Bot} ->
            after_login(Bot);
        Other ->
            ?DBG({oh_noes, Other}),
            init:stop()
    end.



after_login(Bot) ->
    Bot:connect(),
    start_chat_logger(Bot),
    Bot.


start_chat_logger(Bot) ->
    spawn(fun() ->
                  Bot:subscribe([chat, im]),
                  chat_logger()
          end).

chat_logger() ->
    receive
        {message, chat, Chat, _Name} ->
            io:format("(~.8s) <~s> ~s~n", 
                      [Chat#chat.type, Chat#chat.fromName, Chat#chat.text]);

        {message, im, #im{type=message_from_agent}=IM, _Name} ->
            io:format("*****IM***** <~s> ~s~n", 
                      [IM#im.fromName, IM#im.text]);

        {message, im, IM, _Name} ->
            io:format("[~s ~s]~n", [IM#im.fromName, IM#im.type]);

        Other ->
            ?DBG({other, Other})
    end,
    chat_logger().



