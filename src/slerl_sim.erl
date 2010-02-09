%%%-------------------------------------------------------------------
%%% File    : slerl_sim.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : UDP Simulator connection
%%%
%%% Created :  7 Feb 2010 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(slerl_sim).

-behaviour(gen_server).

-include("slerl.hrl").
-include("slerl_util.hrl").

%% API
-export([start_link/2, parse_packet/2]).

%% gen_fsm callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(PORT, 0).
-define(MTU, 1200). % It's what libOMV uses.
-define(SOCKET_OPTIONS, [binary, {active, true}]).

-record(state, {
  info,
  sim,
  socket,
  sequence=0,

  pendingPackets,
  queuedAcks
  
}).


%%====================================================================
%% API
%%====================================================================

start_link(Info, Sim) ->
    gen_server:start_link(?MODULE, [Info, Sim], []).


%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Info, Sim]) ->
    {ok, Socket} = gen_udp:open(?PORT, ?SOCKET_OPTIONS),

    State = #state{info=Info, 
                   sim=Sim, 
                   socket=Socket,
                   sequence=1,
                   pendingPackets=gb_trees:empty(),
                   queuedAcks=[]},

    NewState = send_connect_packets(State),
    self() ! quicktest,

    {ok, NewState}.


handle_call(_Msg, _From, State) ->
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({udp, Socket, IP, Port, Packet}, 
            #state{socket=Socket, sim=#sim{ip=IP, port=Port}}=State) ->
    io:format("~p~n", [Packet]),
    {noreply, State};

handle_info({udp, Socket, IP, Port, Packet}, State) ->
    ?DBG({udp_mismatch, Socket, IP, Port, State, Packet}),
    {noreply, State};

handle_info(Info, State) ->
    ?DBG({unexpected_info, Info}),
    {no_reply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

send(Packet, State) ->
    Sim = State#state.sim,
    ?DBG({send, Sim, Packet}),
    ok = gen_udp:send(State#state.socket, 
                      Sim#sim.ip, Sim#sim.port,
                      list_to_binary(Packet)).


reliable(M) -> M#message{reliable=true}.
    

bool(1) -> true;
bool(0) -> false.
     
unbool(true) -> 1;
unbool(false) -> 0.
     

make_flags(Bits) ->
    [Zero, Reliable, Resend, Acks] = lists:map(fun unbool/1, Bits),
    <<Zero:1/integer-unit:1, Reliable:1/integer-unit:1,
     Resend:1/integer-unit:1, Acks:1/integer-unit:1,
     0:4/integer-unit:1>>.

build_packet(Message, State) ->
    Spec = Message#message.spec,
    Flags = make_flags([Spec#messageDef.zerocoded,
                        Message#message.reliable,
                        Message#message.resend,
                        false]),
    
    Seq = State#state.sequence,
    Header = [Flags, <<Seq:4/integer-unit:8, 0:1/integer-unit:8>>],
    Packet = [Header,Message#message.message],
    {Packet, State#state{sequence=Seq+1}}.




parse_packet(<<Zeroed:1/integer-unit:1, Reliable:1/integer-unit:1,
                Resent:1/integer-unit:1, _Acks:1/integer-unit:1,
                0:4/integer-unit:1,
                _Sequence:4/integer-unit:8, ExtraHeader:1/integer-unit:8,
                Rest/binary>>, _State) ->
    Skeleton  = #message{zerocoded=bool(Zeroed), reliable=bool(Reliable), resend=bool(Resent)},
    {Message,Tail} = parse_packet2(ExtraHeader, Rest, Skeleton),
    ?DBG({Message, Tail}),
    ok.
    

parse_packet2(0, Rest, Message) ->
    slerl_message:parse_message(Rest, Message);
parse_packet2(Count, Packet, Message) ->
    <<Extra:Count/binary, Rest/binary>> = Packet,
    slerl_message:parse_message(Rest, Message#message{extra=Extra}).


send_connect_packets(State) ->
    State1 = use_circuit_code(State),
    State2 = complete_agent_movement(State1),
    %State3 = agent_update(State2),
    %State4 = ping(State3),
    State2.

use_circuit_code(State) ->    
    Sim = State#state.sim,
    Code = Sim#sim.circuitCode,

    Message = slerl_message:build_message(
                'UseCircuitCode',
                [ [Code, Sim#sim.sessionID, Sim#sim.agentID] ]),

    {Packet, State1} = build_packet(reliable(Message), State),    
    send(Packet, State1),
    State1.
    

complete_agent_movement(State) ->
    Sim = State#state.sim,
    Code = Sim#sim.circuitCode,

    Message = slerl_message:build_message(
                'CompleteAgentMovement',
                [ [Sim#sim.agentID, Sim#sim.sessionID, Code ] ]),
    
    {Packet, State1} = build_packet(reliable(Message), State),    
    send(Packet, State1),
    State1.


agent_update(State) ->
    Sim = State#state.sim,

    Message = slerl_message:build_message(
                'AgentUpdate',
                [ [Sim#sim.agentID, Sim#sim.sessionID,
                   {0.0, 0.0, 0.0}, {0.0, 0.0, 0.0},
                   0,
                   {9.388699710976274e-44, 9.388699710976274e-44, 5.748826949892562e-41},
                   {46171676672.0, -1.6316734868642859e-24, 0.0},
                   {-2.4897361554936003e-29, 180356064.0, 0.0},
                   {0.0, 1.793662034335766e-43, -1.658270695400354e35},
                   9.388699710976274e-44,
                   0, 0
                  ] ]),
    
    {Packet, State1} = build_packet(reliable(Message), State),    
    send(Packet, State1),
    State1.
    

ping(State) ->
    Message = slerl_message:build_message(
                'StartPingCheck', [[0, 0]]),
    
    {Packet, State1} = build_packet(Message, State),    
    send(Packet, State1),
    State1.
