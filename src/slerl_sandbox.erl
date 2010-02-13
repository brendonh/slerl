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
                  Bot:subscribe(chat),
                  chat_logger()
          end).

chat_logger() ->
    receive
        {message, chat, Chat, _Name} ->
            Message = Chat#chat.message,
            Type = convert_chat_code(slerl_util:get_field(['ChatData', 'ChatType'], Message#message.message)),
            io:format("(~.8s) <~s> ~s~n", [Type, Chat#chat.fromName, Chat#chat.text]);
        Other ->
            ?DBG({other, Other})
    end,
    chat_logger().


convert_chat_code(0) -> whisper;
convert_chat_code(1) -> say;
convert_chat_code(2) -> shout.
    
