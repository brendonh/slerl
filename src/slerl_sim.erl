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
-export([start_link/2, start_connect/1]).

%% gen_fsm callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(MTU, 1200). 
-define(ACK_PACKET_TIMER, 500).

-define(PORT, 0).
-define(SOCKET_OPTIONS, [binary, {active, true}]).

-record(state, {
  info,
  sim,
  socket,
  sequence,
  pendingPackets,
  queuedAcks,
  ackPacketTimer
}).


%%====================================================================
%% API
%%====================================================================

start_link(Info, Sim) ->
    gen_server:start_link(?MODULE, [Info, Sim], []). 


start_connect(Sim) ->
    gen_server:cast(Sim, start_connect).
   

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

    {ok, State}.


handle_call(_Msg, _From, State) ->
    {reply, ok, State}.


handle_cast(start_connect, State) ->
    {noreply, send_connect_packets(State)};
handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(ack_packet_timer, State) ->
    {noreply, send_ack_packet(State)};

handle_info({udp, Socket, IP, Port, Packet}, 
            #state{socket=Socket, sim=#sim{ip=IP, port=Port}}=State) ->
    NewState = handle_packet(Packet, State),
    {noreply, NewState};

handle_info({udp, Socket, IP, Port, Packet}, State) ->
    ?DBG({udp_mismatch, Socket, IP, Port, State, Packet}),
    {noreply, State};

handle_info(Info, State) ->
    ?DBG({unexpected_info, Info}),
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%%--------------------------------------------------------------------
%%% Receiving
%%--------------------------------------------------------------------  

bool(1) -> true;
bool(0) -> false.


handle_packet(Packet, State) ->
    try parse_packet(Packet, State) of
        Message -> 
            Spec = Message#message.spec,
            ?DBG({got, Spec#messageDef.name}),
            handle_message(Message, State)
    catch Type:Error ->
                        ?DBG({error, Type, Error}),
                        State
                end.


handle_message(Message, State) ->

    case Message#message.reliable of
        true -> 
            Acks = State#state.queuedAcks,
            NewAcks = [Message#message.sequence|Acks],
            State#state{queuedAcks=NewAcks};
        false ->
            State
    end.


parse_packet(<<Zeroed:1/integer-unit:1, Reliable:1/integer-unit:1,
                Resent:1/integer-unit:1, HasAcks:1/integer-unit:1,
                0:4/integer-unit:1,
                Sequence:4/integer-unit:8, ExtraHeader:1/integer-unit:8,
                Rest/binary>>, _State) ->
    {MessageBlock, AckBlock} = split_packet(bool(HasAcks), Rest),
    
    Acks = parse_acks(AckBlock, []),

    case Acks of
        [] -> ok;
        _ -> ?DBG({got_acks, Acks})
    end,

    Skeleton  = #message{
      zerocoded=bool(Zeroed), 
      reliable=bool(Reliable), 
      resend=bool(Resent),
      sequence=Sequence},
    parse_message(ExtraHeader, MessageBlock, Skeleton).


split_packet(false, Packet) ->
    {Packet, <<>>};
split_packet(true, Packet) ->
    BS = byte_size(Packet) - 1,
    <<Tail:BS/binary, AckCount:1/integer-unit:8>> = Packet,
    split_binary(Tail, BS - (AckCount * 4)).

    
parse_message(0, Rest, Message) ->
    slerl_message:parse_message(Rest, Message);
parse_message(Count, Packet, Message) ->
    <<Extra:Count/binary, Rest/binary>> = Packet,
    slerl_message:parse_message(Rest, Message#message{extra=Extra}).


parse_acks(<<>>, Buff) -> lists:reverse(Buff);
parse_acks(<<X:1/integer-unit:32, Rest/binary>>, Buff) ->
    parse_acks(Rest, [X|Buff]);
parse_acks(_Other, _) -> []. % Screw it.


update_ack_timer(#state{ackPacketTimer=none}=State) ->
    {ok, TRef} = timer:send_after(?ACK_PACKET_TIMER, ack_packet_timer),
    State#state{ackPacketTimer=TRef};
update_ack_timer(#state{ackPacketTimer=OldTRef}=State) ->
    timer:cancel(OldTRef),
    {ok, TRef} = timer:send_after(?ACK_PACKET_TIMER, ack_packet_timer),
    State#state{ackPacketTimer=TRef}.



%%====================================================================
%% Sending
%%====================================================================

unbool(true) -> 1;
unbool(false) -> 0.


reliable(M) -> M#message{reliable=true}.


assign_sequence(Message, State) ->
    Seq = State#state.sequence,
    {Message#message{sequence=Seq}, State#state{sequence=Seq+1}}.


dispatch_message(Message, true, State) ->
    {RelMessage, NewState} = assign_sequence(reliable(Message), State),
    NewState2 = dispatch_message(RelMessage, NewState),
    Pending = NewState2#state.pendingPackets,    
    NewPending = gb_trees:insert(RelMessage#message.sequence, RelMessage, Pending),
    ?DBG({pending_packet, RelMessage#message.sequence}),
    NewState2#state{pendingPackets=NewPending};
dispatch_message(Message, false, State) ->
    {M, S} = assign_sequence(Message, State),
    dispatch_message(M, S).


dispatch_message(Message, State) ->
    Spec = Message#message.spec,
    ?DBG({sending, Spec#messageDef.name}),
    {Packet, NewState} = build_packet(Message, State),
    send(Packet, NewState).
    

send(Bin, State) ->
    Sim = State#state.sim,
    ok = gen_udp:send(State#state.socket, 
                      Sim#sim.ip, Sim#sim.port,
                      Bin),
    update_ack_timer(State).


build_packet(Message, State) ->
    Spec = Message#message.spec,
    Flags = make_flags([Spec#messageDef.zerocoded,
                        Message#message.reliable,
                        Message#message.resend,
                        false]),
    
    Seq = Message#message.sequence,
    Header = [Flags, <<Seq:4/integer-unit:8, 0:1/integer-unit:8>>],
    Packet = list_to_binary([Header,Message#message.message]),
    BS = byte_size(Packet),
    {AckSuffix, NewState} = ack_suffix(BS, 0, [], State),
    {list_to_binary([Packet, AckSuffix]), NewState#state{sequence=Seq+1}}.


make_flags(Bits) ->
    [Zero, Reliable, Resend, Acks] = lists:map(fun unbool/1, Bits),
    <<Zero:1/integer-unit:1, Reliable:1/integer-unit:1,
     Resend:1/integer-unit:1, Acks:1/integer-unit:1,
     0:4/integer-unit:1>>.


ack_suffix(BS, Count, Bits, #state{queuedAcks=Acks}=State) 
  when BS == ?MTU orelse Count == 255 orelse Acks == [] ->
    {[<<Count:1/integer-unit:8>>|lists:reverse(Bits)], State};
ack_suffix(BS, Count, Bits, #state{queuedAcks=[Ack|Rest]}=State) ->
    ack_suffix(BS+4, Count+1, [<<Ack:1/integer-unit:32>>|Bits], State#state{queuedAcks=Rest}).



%%====================================================================
%% Timer callbacks
%%====================================================================

send_ack_packet(#state{queuedAcks=[]}=State) -> State;
send_ack_packet(#state{queuedAcks=Acks}=State) ->
    AckGroups = [ [A] || A <- lists:reverse(Acks) ],
    Message = slerl_message:build_message('PacketAck', [AckGroups]),
    ?DBG({sending_ack_packet, Acks}),
    dispatch_message(Message, true, State#state{queuedAcks=[]}).
     


%%====================================================================
%% Connect
%%====================================================================


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

    dispatch_message(Message, true, State).
    

complete_agent_movement(State) ->
    Sim = State#state.sim,
    Code = Sim#sim.circuitCode,

    Message = slerl_message:build_message(
                'CompleteAgentMovement',
                [ [Sim#sim.agentID, Sim#sim.sessionID, Code ] ]),
    
    dispatch_message(Message, true, State).


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
    
    dispatch_message(Message, true, State).
    

ping(State) ->
    Message = slerl_message:build_message(
                'StartPingCheck', [[0, 0]]),
    
    {Packet, State1} = build_packet(Message, State),    
    send(Packet, State1),
    State1.
