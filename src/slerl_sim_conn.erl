%%%-------------------------------------------------------------------
%%% File    : slerl_sim.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : UDP Simulator connection
%%%
%%% Created :  7 Feb 2010 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(slerl_sim_conn).

-behaviour(gen_server).

-include("slerl.hrl").
-include("slerl_util.hrl").

%% API
-export([start_link/2, start_connect/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% These values generally follow libOMV settings.
-define(MTU, 1200).
-define(ACK_PACKET_TIMER, 500).
-define(RESEND_PACKET_TIMER, 4000).
-define(MAX_RESENDS, 2).
-define(PING_INTERVAL, 5000). % Down from libOMV's 2200

% Number of pings to average
-define(PING_WINDOW, 5).

-define(RESEND_TIMER_MICROSECONDS, ?RESEND_PACKET_TIMER * 1000).

-define(PORT, 0).
-define(SOCKET_OPTIONS, [binary, {active, true}]).

-define(CLEAN_SUBSCRIPTIONS_INTERVAL, 60000).

-define(DEBUG, false). % Puts us straight into trace mode
%-define(DEBUG_IGNORE, ['PacketAck', 'ViewerEffect', 'PreloadSound', 'AttachedSound']).
-define(TRACE(T), if State#state.trace -> ?DBG(T); true -> ok end).

-record(state, {
  name,
  simInfo,

  socket,
  sequence,
  pendingPackets,
  queuedAcks,

  ackTimer,
  resendTimer,

  pingID,
  pendingPings,
  pingWindow,

  subscriptions,

  trace
}).


%%====================================================================
%% API
%%====================================================================

start_link(Name, SimInfo) ->
    gen_server:start_link(?MODULE, [Name, SimInfo], []).


start_connect(Pid) ->
    gen_server:cast(Pid, start_connect).


%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Name, SimInfo]) ->

    process_flag(trap_exit, true),

    {ok, Socket} = gen_udp:open(?PORT, ?SOCKET_OPTIONS),

    State = #state{name=Name,
                   simInfo=SimInfo,
                   socket=Socket,
                   sequence=1,
                   pendingPackets=gb_trees:empty(),
                   queuedAcks=[],
                   resendTimer=none,
                   ackTimer=none,
                   pendingPings=[],
                   pingID=1,
                   pingWindow=[],
                   subscriptions=dict:new(),
                   trace=?DEBUG
                  },

    ets:insert(Name, {{udp, SimInfo#sim.ip, SimInfo#sim.port}, self()}),

    timer:send_interval(?PING_INTERVAL, ping_timer),
    timer:send_interval(?CLEAN_SUBSCRIPTIONS_INTERVAL, clean_subscriptions_timer),

    {ok, State}.



handle_call(get_ping, _From, State) ->
    Window = State#state.pingWindow,
    Reply = lists:sum(Window) / (length(Window)*1000),
    {reply, Reply, State};
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.


handle_cast({subscribe, MessageName, Proc}, State) ->
    NewSubs = dict:append(MessageName, Proc, State#state.subscriptions),
    {noreply, State#state{subscriptions=NewSubs}};

handle_cast({unsubscribe, MessageName, Proc}, State) ->
    Subs = State#state.subscriptions,
    NewSubs = case dict:find(MessageName, Subs) of
                  error -> Subs;
                  {ok, OldList} ->
                      NewList = [P || P <- OldList, P /= Proc],
                      dict:store(MessageName, NewList, Subs)
              end,
    {noreply, State#state{subscriptions=NewSubs}};

handle_cast({send, Message, Reliable}, State) ->
    {noreply, send_message(Message, Reliable, State)};

handle_cast({trace, Trace}, State) when is_boolean(Trace) ->
    {noreply, State#state{trace=Trace}};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(ack_packet_timer, State) ->
    {noreply, send_ack_packet(State#state{ackTimer=none})};

handle_info(resend_packet_timer, State) ->
    {noreply, resend_packets(State#state{resendTimer=none})};

handle_info(clean_subscriptions_timer, State) ->
    ?TRACE(cleaning_subscriptions),
    Subs = State#state.subscriptions,
    NewSubsList = dict:fold(
                    fun(K, V, A) ->
                            V2 = [P || P <- V, is_process_alive(P)],
                            [{K,V2}|A]
                    end, [], Subs),
    {noreply, State#state{subscriptions=dict:from_list(NewSubsList)}};

handle_info(ping_timer, State) ->
    {noreply, send_ping(State)};

handle_info({udp, Socket, IP, Port, Packet},
            #state{socket=Socket, simInfo=#sim{ip=IP, port=Port}}=State) ->
    NewState = handle_packet(Packet, State),
    {noreply, NewState};

handle_info({udp, Socket, IP, Port, Packet}, State) ->
    ?TRACE({udp_mismatch, Socket, IP, Port, State, Packet}),
    {noreply, State};

handle_info(Info, State) ->
    ?TRACE({unexpected_info, Info}),
    {noreply, State}.


terminate(_Reason, State) ->
    gen_udp:close(State#state.socket),
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
        {Message, Acks} ->
            Spec = Message#message.spec,
            NewState = process_acks(Acks, State),
            ?TRACE({got, Message#message.sequence, Spec#messageDef.name}),
            handle_message(Message, NewState)
    catch Type:Error ->
                        ?TRACE({error, Type, Error}),
                        State
                end.


handle_message(Message, State) ->
    NewState = case Message#message.reliable of
        true ->
            Acks = State#state.queuedAcks,
            NewAcks = [Message#message.sequence|Acks],
            update_ack_timer(State#state{queuedAcks=NewAcks});
        false ->
            State
    end,
    Spec = Message#message.spec,
    dispatch_message(Spec#messageDef.name, Message, NewState).


parse_packet(<<Zeroed:1/integer-unit:1, Reliable:1/integer-unit:1,
                Resent:1/integer-unit:1, HasAcks:1/integer-unit:1,
                0:4/integer-unit:1,
                Sequence:4/integer-unit:8, ExtraHeader:1/integer-unit:8,
                Rest/binary>>, _State) ->
    {MessageBlock, AckBlock} = split_packet(bool(HasAcks), Rest),

    Acks = parse_acks(AckBlock, []),

    Skeleton  = #message{
      zerocoded=bool(Zeroed),
      reliable=bool(Reliable),
      resend=bool(Resent),
      sequence=Sequence},

    Message =  parse_message(ExtraHeader, MessageBlock, Skeleton),
    {Message, Acks}.



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


process_acks([], State) ->
    State;
process_acks([Ack|Rest], State) ->
    ?TRACE({acked, Ack}),
    Pending = State#state.pendingPackets,
    NewPending = gb_trees:delete_any(Ack, Pending),
    NewState = State#state{pendingPackets=NewPending},
    process_acks(Rest, NewState).


%%====================================================================
%% Sending
%%====================================================================

unbool(true) -> 1;
unbool(false) -> 0.


reliable(M) -> M#message{reliable=true}.


assign_sequence(Message, State) ->
    Seq = State#state.sequence,
    {Message#message{sequence=Seq}, State#state{sequence=Seq+1}}.


send_message(Message, true, State) ->
    {RelMessage, NewState} = assign_sequence(reliable(Message), State),
    NewState2 = send_message(RelMessage, NewState),
    Pending = NewState2#state.pendingPackets,
    SentCount = RelMessage#message.sentCount,
    NewPending = gb_trees:insert(RelMessage#message.sequence,
                                 RelMessage#message{sentCount=SentCount+1,
                                                    lastSent=now()},
                                 Pending),
    ?TRACE({pending_packet, RelMessage#message.sequence}),
    update_resend_timer(NewState2#state{pendingPackets=NewPending});
send_message(Message, false, State) ->
    {M, S} = assign_sequence(Message, State),
    send_message(M, S).


send_message(Message, State) ->
    Spec = Message#message.spec,
    {Packet, NewState} = build_packet(Message, State),
    ?TRACE({sending, Spec#messageDef.name}),
    send(Packet, NewState).


send(Bin, State) ->
    Sim = State#state.simInfo,
    ok = gen_udp:send(State#state.socket,
                      Sim#sim.ip, Sim#sim.port,
                      Bin),
    update_ack_timer(State).


build_packet(Message, State) ->
    Spec = Message#message.spec,
    Seq = Message#message.sequence,

    BS = 6 + byte_size(Message#message.message),
    {AckSuffix, NewState} = ack_suffix(BS, 0, [], State),

    Flags = make_flags([Spec#messageDef.zerocoded,
                        Message#message.reliable,
                        Message#message.resend,
                        AckSuffix /= []]),

    Header = [Flags, <<Seq:4/integer-unit:8, 0:1/integer-unit:8>>],
    Packet = list_to_binary([Header,Message#message.message,AckSuffix]),

    {Packet, NewState#state{sequence=Seq+1}}.


make_flags(Bits) ->
    [Zero, Reliable, Resend, Acks] = lists:map(fun unbool/1, Bits),
    <<Zero:1/integer-unit:1, Reliable:1/integer-unit:1,
     Resend:1/integer-unit:1, Acks:1/integer-unit:1,
     0:4/integer-unit:1>>.


ack_suffix(BS, Count, Bits, #state{queuedAcks=Acks}=State)
  when BS == ?MTU orelse Count == 255 orelse Acks == [] ->
    {lists:reverse([<<Count:1/integer-unit:8>>|Bits]), State};
ack_suffix(BS, Count, Bits, #state{queuedAcks=[Ack|Rest]}=State) ->
    ack_suffix(BS+4, Count+1, [<<Ack:1/integer-unit:32>>|Bits], State#state{queuedAcks=Rest}).


%%====================================================================
%% Timers
%%====================================================================

update_ack_timer(#state{ackTimer=none}=State) ->
    {ok, TRef} = timer:send_after(?ACK_PACKET_TIMER, ack_packet_timer),
    State#state{ackTimer=TRef};
update_ack_timer(#state{ackTimer=OldTRef}=State) ->
    timer:cancel(OldTRef),
    {ok, TRef} = timer:send_after(?ACK_PACKET_TIMER, ack_packet_timer),
    State#state{ackTimer=TRef}.


send_ack_packet(#state{queuedAcks=[]}=State) -> State;
send_ack_packet(#state{queuedAcks=Acks}=State) ->
    %% Should we max this out at 255?
    AckGroups = [ [A] || A <- lists:reverse(Acks) ],
    Message = slerl_message:build_message('PacketAck', [AckGroups]),
    send_message(Message, true, State#state{queuedAcks=[]}).



update_resend_timer(#state{resendTimer=none}=State) ->
    ?TRACE(setting_resend_timer),
    {ok, TRef} = timer:send_after(?RESEND_PACKET_TIMER, resend_packet_timer),
    State#state{resendTimer=TRef};
update_resend_timer(State) ->
    ?TRACE(resend_timer_exists),
    State.


resend_packets(#state{pendingPackets=Pending}=State) ->
    Now = now(),
    resend_expired(gb_trees:to_list(Pending), Now,
                   State#state{pendingPackets=gb_trees:empty()}).


resend_expired([], _Now, State) ->
    State;
resend_expired([{_ID, #message{lastSent=LS}=M}|Rest], Now, State) ->
    TD = timer:now_diff(Now, LS),
    if TD >= ?RESEND_TIMER_MICROSECONDS ->
            if M#message.sentCount > ?MAX_RESENDS ->
                    ?TRACE({packet_lost, M}),
                    resend_expired(Rest, Now, State);
               true ->
                    NewMessage = M#message{lastSent=now(),
                                           sentCount=M#message.sentCount,
                                           resend=true},
                    ?TRACE({resending, NewMessage}),
                    NewState = send_message(NewMessage, true, State),
                    resend_expired(Rest, Now, NewState)
            end;
       true ->
            Pending = State#state.pendingPackets,
            NewPending = gb_trees:insert(M#message.sequence, M, Pending),
            NewState = State#state{pendingPackets=NewPending},
            resend_expired(Rest, Now, NewState)
    end.


send_ping(State) ->
    PingID = State#state.pingID,
    Unacked = gb_trees:keys(State#state.pendingPackets),
    OldestUnacked = case Unacked of
                        [] -> 0;
                        _ -> lists:min(Unacked)
                    end,

    Message = slerl_message:build_message(
                'StartPingCheck', [[PingID, OldestUnacked]]),

    NewState = send_message(Message, false, State),
    NewState#state{
      pendingPings=[{PingID, now()}|NewState#state.pendingPings],
      pingID=PingID+1
     }.


%%====================================================================
%% Dispatch
%%====================================================================

dispatch_message('PacketAck', Message, State) ->
    [{_, Groups}] = Message#message.message,
    IDs = [ID || [{_, ID}] <- Groups],
    process_acks(IDs, State);

dispatch_message('StartPingCheck', Message, State) ->
    [{_,[{_,PingID},{_, OldestUnacked}]}] = Message#message.message,
    Reply = slerl_message:build_message('CompletePingCheck', [[PingID]]),
    NewState = send_message(Reply, false, State),
    remove_old_acks(OldestUnacked, NewState);

dispatch_message('CompletePingCheck', Message, State) ->
    [{_, [{_, PingID}]}] = Message#message.message,
    {SentTime, NewPending} = find_ping(PingID, State#state.pendingPings),

    NewWindow = case SentTime of
                    none ->
                        State#state.pingWindow;
                    _ ->
                        Diff = timer:now_diff(now(), SentTime),
                        trim_pings([Diff|State#state.pingWindow], [], ?PING_WINDOW)
                end,
    ?TRACE({average_ping, lists:sum(NewWindow) / (length(NewWindow)*1000)}),
    State#state{pendingPings=NewPending, pingWindow=NewWindow};

dispatch_message('RegionHandshake', #message{message=Message}=Orig, State) ->
    SimName = slerl_util:extract_string(
                slerl_util:get_field(['RegionInfo', 'SimName'], Message)),

    RegionID = slerl_util:get_field(['RegionInfo2', 'RegionID'], Message),

    SimInfo = State#state.simInfo,
    NewInfo = SimInfo#sim{name=SimName, regionID=RegionID},
    NewState = State#state{simInfo=NewInfo},

    Reply = slerl_message:build_message(
              'RegionHandshakeReply',
              [[NewInfo#sim.agentID, NewInfo#sim.sessionID],
               [0]]),

    NewState2 = send_message(Reply, true, NewState),
    broadcast_message('RegionHandshake', Orig, NewState2);

dispatch_message(Name, Message, State) ->
    broadcast_message(Name, Message, State).


broadcast_message(Name, Message, State) ->
    case dict:find(Name, State#state.subscriptions) of
        error -> ok;
        {ok, Ps} ->
            Self = {self(), State#state.simInfo},
            Msg = {message, Name, Message, Self},
            [P ! Msg || P <- Ps]
    end,
    State.


remove_old_acks(Oldest, #state{pendingPackets=Pending}=State) ->
    NewPending = [{ID, M} || {ID, M} <- gb_trees:to_list(Pending),
                             ID >= Oldest],
    State#state{pendingPackets=gb_trees:from_orddict(NewPending)}.


find_ping(PingID, Pending) ->
    find_ping(PingID, Pending, [], Pending).

find_ping(_, [], _, Orig) -> {none, Orig};
find_ping(PingID, [{PingID,Time}|_], Buff, _) -> {Time, lists:reverse(Buff)};
find_ping(PingID, [Other|Rest], Buff, Orig) -> find_ping(PingID, Rest, [Other|Buff], Orig).

trim_pings([], Buff, _) -> lists:reverse(Buff);
trim_pings(_, Buff, Count) when Count == 0 -> lists:reverse(Buff);
trim_pings([P|Rest], Buff, Count) -> trim_pings(Rest, [P|Buff], Count-1).


