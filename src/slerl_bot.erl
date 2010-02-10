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
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
  name,
  info,
  sup,
  currentSim
}).

%%====================================================================
%% API
%%====================================================================

start_link(Name, Info, Sup) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name, Info, Sup], []).


%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Name, Info, Sup]) ->
    ets:new(Name, [set, public, named_table]),
    {ok, #state{name=Name, info=Info, sup=Sup}}.


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

    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
