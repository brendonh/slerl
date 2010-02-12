%%%-------------------------------------------------------------------
%%% File    : slerl_sim.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : High-level sim connection management
%%%
%%% Created : 10 Feb 2010 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(slerl_sim).

-behaviour(gen_server).

-include("slerl.hrl").
-include("slerl_util.hrl").

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
  name,
  simInfo,
  connKey
}).

%%====================================================================
%% API
%%====================================================================

start_link(Name, SimInfo) ->
    gen_server:start_link(?MODULE, [Name, SimInfo], []).


%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Name, SimInfo]) ->
    process_flag(trap_exit, true),

    ?DBG({sim_starting, SimInfo#sim.ip, SimInfo#sim.port}),

    State = #state{
      name=Name,
      simInfo=SimInfo,
      connKey={udp, SimInfo#sim.ip, SimInfo#sim.port}
     },

    ets:insert(Name, {{sim, SimInfo#sim.ip, SimInfo#sim.port}, self()}),

    {ok, State}.

%% --------------------------------------------

handle_call({subscribe, MessageName}, {Pid, _}, State) -> 
    gen_server:cast(get_conn(State), {subscribe, MessageName, Pid}),
    {reply, ok, State};

handle_call({unsubscribe, MessageName}, {Pid, _}, State) -> 
    gen_server:cast(get_conn(State), {unsubscribe, MessageName, Pid}),
    {reply, ok, State};

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

%% --------------------------------------------

handle_cast(start_connect, State) ->
    send_connect_packets(State),
    {noreply, State};

handle_cast({send, Message, Reliable}, State) ->
    send_message(Message, Reliable, State),
    {noreply, State};

handle_cast({seed_capabilities, CapsList}, State) ->
    Caps = dict:from_list(CapsList),
    NewInfo = (State#state.simInfo)#sim{caps=Caps},

    case dict:find('EventQueueGet', Caps) of
        error ->  ?DBG(no_event_queue);
        {ok, _} -> slerl_sim_group_sup:start_event_queue(State#state.name, NewInfo)
    end,

    {noreply, State#state{simInfo=NewInfo}};

handle_cast(logout, State) ->
    logout(State),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------

handle_info({message, 'AgentMovementComplete', Message, Conn}, State) ->
    agent_movement_complete(Message, Conn, State);

handle_info({message, 'LogoutReply', Message, Conn}, State) ->
    logged_out(Message, Conn, State);

handle_info({message, 'RegionHandshake', _Message, _Conn}, State) ->
    request_seed_caps(State),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------

terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

get_conn(#state{name=Name, connKey=ConnKey}) ->
    ets:lookup_element(Name, ConnKey, 2).


send_message(Message, Reliable, State) ->
    gen_server:cast(get_conn(State), {send, Message, Reliable}).


subscribe(MessageName, State) ->
    gen_server:cast(get_conn(State), {subscribe, MessageName, self()}).


bot_cast(Message, State) ->
    Bot = ets:lookup_element(State#state.name, bot, 2),
    gen_server:cast(Bot, Message).


request_seed_caps(State) ->
    SimInfo = State#state.simInfo,
    URL = SimInfo#sim.seedCapability,
    Self = self(),
    spawn(fun() -> 
       case slerl_caps:request_seed_caps(URL) of
           {ok, Caps} -> gen_server:cast(Self, {seed_capabilities, Caps});
           _ -> ok
       end
          end).


%%====================================================================
%% Connect Sequence
%%====================================================================

send_connect_packets(State) ->
    subscribe('RegionHandshake', State),
    subscribe('AgentMovementComplete', State),
    subscribe('LogoutReply', State),
    use_circuit_code(State),
    complete_agent_movement(State).


use_circuit_code(State) ->    
    Sim = State#state.simInfo,
    Code = Sim#sim.circuitCode,

    Message = slerl_message:build_message(
                'UseCircuitCode',
                [ [Code, Sim#sim.sessionID, Sim#sim.agentID] ]),

    send_message(Message, true, State).
    

complete_agent_movement(State) ->
    Sim = State#state.simInfo,
    Code = Sim#sim.circuitCode,

    Message = slerl_message:build_message(
                'CompleteAgentMovement',
                [ [Sim#sim.agentID, Sim#sim.sessionID, Code ] ]),
    
    send_message(Message, true, State).


logout(State) ->
    Sim = State#state.simInfo,
    Message = slerl_message:build_message(
                'LogoutRequest',
                [ [Sim#sim.agentID, Sim#sim.sessionID] ]),
    send_message(Message, true, State),
    close_circuit(State).


close_circuit(State) ->
    Message = slerl_message:build_message('CloseCircuit', []),
    % Should this *really* be reliable? It is in libOMV
    send_message(Message, true, State).
    

%%====================================================================
%% Message forwarding
%%====================================================================

agent_movement_complete(_Message, {_Conn, SimInfo}, State) ->
    bot_cast({simulator, region_changed, SimInfo}, State),
    {noreply, State}.

logged_out(_Message, {_Conn, SimInfo}, State) ->
    bot_cast({simulator, logged_out, SimInfo}, State),
    {noreply, State}.


