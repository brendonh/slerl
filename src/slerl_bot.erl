%%%-------------------------------------------------------------------
%%% File    : slerl_bot.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : High-level bot API
%%%
%%% Created : 10 Feb 2010 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(slerl_bot).

-behaviour(gen_server).

-include("slerl.hrl").
-include("slerl_util.hrl").

%% API
-export([start_link/3, logout/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(LOGOUT_TIMEOUT, 5000).

-record(state, {
  name,
  info,
  sup,
  currentSim=none,
  simInfo=none
}).

%%====================================================================
%% API
%%====================================================================

start_link(Name, Info, Sup) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name, Info, Sup], []).

logout(Bot) ->
    gen_server:cast(Bot, logout).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Name, Info, Sup]) ->
    ets:new(Name, [set, public, named_table]),
    ets:insert(Name, {bot, self()}),
    {ok, #state{name=Name, info=Info, sup=Sup}}.



handle_call({get_region, Name}, From, State) ->   
    spawn(fun() -> region_getter(Name, From, State) end),
    {noreply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


handle_cast(initial_connect, State) ->

    I = fun(K) -> ?GV(K, State#state.info) end,

    AgentID = slerl_util:parse_uuid(I("agent_id")),
    SessionID = slerl_util:parse_uuid(I("session_id")),

    SimInfo = #sim{
      ip=slerl_util:parse_ip(I("sim_ip")), 
      port=list_to_integer(I("sim_port")),
      circuitCode=list_to_integer(I("circuit_code")),
      regionPos={I("region_x"), I("region_y")},
      seedCapability=I("seed_capability"),
      agentID=AgentID,
      sessionID=SessionID}, 

    slerl_sim_sup:start_sim_group(State#state.name, SimInfo),

    {noreply, State};

handle_cast({{simulator, region_changed, Sim}, {_Message, SimInfo}}, State) ->
    ?DBG({region_changed, SimInfo#sim.name}),
    {noreply, State#state{currentSim=Sim, simInfo=SimInfo}};

handle_cast({{simulator, logged_out, _Sim}, {_Message, _SimInfo}}, State) ->
    ?DBG(logged_out),
    exit(State#state.sup, shutdown),
    {noreply, State};

handle_cast(logout, State) ->
    do_logout(State),
    {noreply, State};

handle_cast({trace, Trace}, State) when is_boolean(Trace) ->
    [gen_server:cast(C, {trace, Trace})
     || [C] <- ets:match('Teania Amaterasu', {{'udp', '_', '_'}, '$1'})],
    {noreply, State};

handle_cast(Other, State) ->
    ?DBG({unexpected_cast, Other}),
    {noreply, State}.


handle_info(logout_timeout, State) ->
    exit(State#state.sup, shutdown),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

do_logout(State) ->          
    ?DBG(logging_out),
    case State#state.currentSim of
        none -> exit(State#state.sup, shutdown);
        Sim -> 
            gen_server:cast(Sim, logout),
            timer:send_after(?LOGOUT_TIMEOUT, logout_timeout)
    end.


region_getter(Name, From, State) ->
    Sim = State#state.currentSim,
    SimInfo = State#state.simInfo,
    gen_server:call(Sim, {subscribe, 'MapBlockReply'}),
    Message = slerl_message:build_message(
                'MapNameRequest', [ [SimInfo#sim.agentID, SimInfo#sim.sessionID, 0, 0, false],
                                    [Name] ]),
    gen_server:cast(Sim, {send, Message, true}),
    receive
        {message, 'MapBlockReply', Response, _Conn} ->
            Reply = case [B || B <- ?GV('Data', Response#message.message),
                               ?GV('Name', B) == Name] of
                        [Block] -> {ok, Block};
                        [] -> {error, not_found};
                        Other -> {error, {multiple_results, Other}}
                    end,
            gen_server:reply(From, Reply);
        _ -> ignore
    after 10000 ->
        fail
    end.
