-module(slerl_sandbox).

-include("slerl.hrl").
-include("slerl_util.hrl").

-export([test/0]).

-define(OWNER, "Brendon Lanley").

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
            ?DBG({oh_noes, Other})
    end.



after_login(Bot) ->
    Bot:connect(),
    start_chat_logger(Bot),
    Bot.


start_chat_logger(Bot) ->
    spawn(fun() ->
                  Bot:subscribe([chat, im]),
                  chat_logger(Bot)
          end).

chat_logger(Bot) ->
    receive
        {message, chat, Chat, _Name} ->
            io:format("(~.8s) <~s> ~s~n", 
                      [Chat#chat.type, Chat#chat.fromName, Chat#chat.text]);


        {message, im, #im{fromName= <<?OWNER>>, 
                          type=message_from_agent}=IM, _Name} ->
            handle_command(Bot, IM#im.text, IM);

        {message, im, #im{type=message_from_agent}=IM, _Name} ->
            io:format("*****IM***** <~s> ~s~n", 
                      [IM#im.fromName, IM#im.text]);

        {message, im, IM, _Name} ->
            io:format("[~s ~s]~n", [IM#im.fromName, IM#im.type]);

        Other -> ?DBG(Other)
    end,
    chat_logger(Bot).


handle_command(Bot, <<"port ", Region/binary>>, _) ->
    Bot:im(?OWNER, list_to_binary(["Porting to ", Region])),
    Bot:teleport(Region, {128, 128, 2500});
handle_command(Bot, <<"heel">>, IM) ->
    Position = slerl_util:get_field(['MessageBlock', 'Position'], IM#im.message),
    {ok, {SimName, _}} = Bot:position(),
    case Position of
        {0.0, 0.0, 0.0} -> Bot:im(?OWNER, "Port me first");
        _ -> 
            Result = Bot:teleport(SimName, Position),
            Bot:im(?OWNER, lists:flatten(io_lib:format("Result: ~p", [Result])))
    end;
handle_command(_, _, _) -> ok.
    
