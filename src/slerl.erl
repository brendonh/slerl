%%%-------------------------------------------------------------------
%%% File    : slerl.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : High-level API
%%%
%%% Created :  7 Feb 2010 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(slerl).

-include("slerl.hrl").
-include("slerl_util.hrl").

-export([login/3, login/4]).


login(First, Last, Password) -> login(First, Last, Password, "last").
     
login(First, Last, Password, Start) ->
    ?DBG({logging_in, First, Last}),
    {ok, URL} = application:get_env(slerl, login_url),
    {ok, VersionInts} = application:get_env(slerl, client_version),
    Version = list_to_tuple([integer_to_list(I) || I <- tuple_to_list(VersionInts)]),
    Response = slerl_login:login(URL, First, Last, Password, Version, Start),
    case Response of
        {ok, Info} -> 
            ?DBG(xmlrpc_login_succeeded),
            start_bot(Info);
        Other -> Other
    end.



start_bot(Info) ->
    ?DBG(parsing_messages),
    Messages = slerl_util:parse_message_template(),
    ?DBG(starting_bot),
    {ok, Pid} = supervisor:start_child(slerl_sup, [Info, Messages]),

    I = fun(K) -> ?GV(K, Info) end,

    SimConnect = #simConnect{
      sim={I("sim_ip"), I("sim_port")},
      circuitCode=I("circuit_code"),
      regionPos={I("region_x"), I("region_y")},
      seedCapability=I("seed_capability")},    
    
    ?DBG({starting_sim, SimConnect}),

    ok.
