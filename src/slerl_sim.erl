%%%-------------------------------------------------------------------
%%% File    : slerl_sim.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : High-level sim connection management
%%%
%%% Created : 10 Feb 2010 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(slerl_sim).

-behaviour(gen_fsm).

-include("slerl.hrl").
-include("slerl_util.hrl").

%% API
-export([start_link/2]).

%% gen_fsm callbacks
-export([init/1, 
         uninitialized/2, uninitialized/3, 
         connecting/2, connecting/3, 
         connected/2, connected/3,
         handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state, {
  name,
  simInfo,
  connKey
}).

%%====================================================================
%% API
%%====================================================================

start_link(Name, SimInfo) ->
    gen_fsm:start_link(?MODULE, [Name, SimInfo], []).


%%====================================================================
%% gen_fsm callbacks
%%====================================================================

init([Name, SimInfo]) ->
    ?DBG({sim_starting, SimInfo#sim.ip, SimInfo#sim.port}),

    State = #state{
      name=Name,
      simInfo=SimInfo,
      connKey={udp, SimInfo#sim.ip, SimInfo#sim.port}
     },

    ets:insert(Name, {{sim, SimInfo#sim.ip, SimInfo#sim.port}, self()}),

    {ok, uninitialized, State}.



uninitialized(start_connect, State) ->
    send_connect_packets(State),
    {next_state, connecting, State}.

uninitialized(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, uninitialized, State}.



connecting({message, 'RegionHandshake', Message, Conn}, State) ->
    handshake_received(Message, Conn, State);

connecting(Event, State) ->
    ?DBG({connecting_event, Event}),
    {next_state, connecting, State}.

connecting(_Event, _From, State) ->
    {reply, ok, connecting, State}.


connected(Event, State) ->
    ?DBG({connected_event, Event}),
    {next_state, connected, State}.

connected(_Event, _From, State) ->
    {reply, ok, connected, State}.


handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.



handle_info({message, _, _, _}=Message, StateName, State) ->
    apply(?MODULE, StateName, [Message, State]); % Hmm.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.


terminate(_Reason, _StateName, _State) ->
    ok.


code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
    
get_conn(#state{name=Name, connKey=ConnKey}) ->
    ets:lookup_element(Name, ConnKey, 2).


send_message(Message, Reliable, State) ->
    gen_server:cast(get_conn(State), {send, Message, Reliable}).


subscribe(MessageName, State) ->
    gen_server:cast(get_conn(State), {subscribe, MessageName, self()}).


%%====================================================================
%% Connect Sequence
%%====================================================================

send_connect_packets(State) ->
    subscribe('RegionHandshake', State),
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


%%====================================================================
%% Arrival
%%====================================================================

handshake_received(Message, _Conn, State) ->
    
    SimName = slerl_util:extract_string(
                slerl_util:get_field(["RegionInfo", "SimName"], 
                                     Message#message.message)),
    
    ?DBG({handshake, SimName}),
    {next_state, connected, State}.
