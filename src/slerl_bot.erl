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

-define(LOGOUT_TIMEOUT, 5000).

-define(CLEAN_SUBSCRIPTIONS_INTERVAL, 60000).

-record(state, {
  name,
  info,
  sup,
  currentSimKey=none,
  simInfo=none,
  blockInfo=none,
  subscriptions
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
    ets:insert(Name, {bot, self()}),
    timer:send_interval(?CLEAN_SUBSCRIPTIONS_INTERVAL, clean_subscriptions_timer),
    {ok, #state{name=Name, info=Info, sup=Sup, subscriptions=dict:new()}}.


%%====================================================================
%% API
%%====================================================================

handle_call({get_region, Name}, From, State) ->
    case current_sim(State) of
        none -> {reply, no_sim, State};
        _ ->
            spawn(fun() -> gen_server:reply(From, region_getter(Name, State)) end),
            {noreply, State}
    end;

handle_call({teleport, RegionName, Position}, From, State) ->
    case current_sim(State) of
        none -> {reply, no_sim, State};
        _ ->
            spawn(fun() -> do_teleport(From, RegionName, Position, State) end),
            {noreply, State}
    end;

handle_call(position, _From, State) ->
    Reply = case current_sim(State) of
                none -> {error, no_sim};
                Sim ->
                    RegionPos = gen_server:call(Sim, position),
                    {ok, {(State#state.simInfo)#sim.name, RegionPos}}
            end,
    {reply, Reply, State};

handle_call({send_chat, _, _, _}=SendChat, _From, State) ->
    Reply = case current_sim(State) of
                none -> {error, no_sim};
                Sim -> gen_server:cast(Sim, SendChat)
            end,
    {reply, Reply, State};

handle_call({find_avatar_uuids, Name}, From, State) ->
    case current_sim(State) of
        none -> 
            {reply, no_sim, State};
        _ -> 
            spawn(fun() -> gen_server:reply(From, avatar_uuid_finder(Name, State)) end),
            {noreply, State}
    end;

handle_call(block, _From, State) ->
    {reply, State#state.blockInfo, State};

handle_call(retrieve_ims, _From, State) ->
    case current_sim(State) of
        none -> Reply = {error, no_sim};
        Sim -> 
            SimInfo = State#state.simInfo,
            Message = slerl_message:build_message(
                        'RetrieveInstantMessages',
                        [ [ SimInfo#sim.agentID, SimInfo#sim.sessionID ] ]),
            gen_server:cast(Sim, {send, Message, true}),
            Reply = ok
    end,
    {reply, Reply, State};

handle_call({send_im, UUID, Text}, _From, State) ->
    send_im(UUID, none, Text, State),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, unknown_call, State}.


%%====================================================================
%% Internal bookkeeping
%%====================================================================

handle_cast(initial_connect, State) ->
    simconnect_from_login(State, State#state.info),
    {noreply, State};

handle_cast({simulator, region_changed, {SimInfo, SimName}}, State) ->
    ?DBG({region_changed, SimName}),
    OldSimInfo = State#state.simInfo,
    NewState = State#state{currentSimKey={sim, SimInfo#sim.ip, SimInfo#sim.port}, simInfo=SimInfo},
    %% spawn(fun() -> map_block_request(Handle, NewState) end),
    
    if OldSimInfo == none orelse ({SimInfo#sim.ip, SimInfo#sim.port} == {OldSimInfo#sim.ip, OldSimInfo#sim.port}) ->
            ok;
       true ->
            OldConn = ets:lookup_element(State#state.name, {udp, OldSimInfo#sim.ip, OldSimInfo#sim.port}, 2),
            gen_server:cast(OldConn, stop_ping)
    end,
    
    {noreply, NewState};

handle_cast({simulator, logged_out, _SimInfo}, State) ->
    ?DBG(logged_out),
    exit(State#state.sup, shutdown),
    {noreply, State};

handle_cast({simulator, defunct, {IP, Port}}, State) ->
    SimInfo = State#state.simInfo,
    Current = {SimInfo#sim.ip, SimInfo#sim.port},
    if {IP, Port} == Current ->
            ?DBG(current_simulator_defunct),
            do_logout(State);
       true ->
            ?DBG({stopping_defunct_sim, IP, Port}),
            kill_sim(State#state.name, IP, Port)
    end,
    {noreply, State};

handle_cast({simulator, Type, Message}, State) ->
    broadcast_message(Type, Message, State),
    {noreply, State};


handle_cast(logout, State) ->
    do_logout(State),
    {noreply, State};

handle_cast({trace, Trace}, State) ->
    [gen_server:cast(C, {trace, Trace})
     || [C] <- ets:match(State#state.name, {{'udp', '_', '_'}, '$1'})],
    {noreply, State};
       

%%--------------------------------------------------------------------
%%% Pubsub
%%--------------------------------------------------------------------

handle_cast({subscribe, MessageNames, Proc}, State)
  when is_list(MessageNames) ->
    NewSubs = lists:foldl(fun(MN, D) -> dict:append(MN, Proc, D) end,
                          State#state.subscriptions, MessageNames),
    {noreply, State#state{subscriptions=NewSubs}};

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


%%--------------------------------------------------------------------
%%% Boilerplate
%%--------------------------------------------------------------------

handle_cast(Other, State) ->
    ?DBG({unexpected_cast, Other}),
    {noreply, State}.


handle_info(clean_subscriptions_timer, State) ->
    Subs = State#state.subscriptions,
    NewSubsList = dict:fold(
                    fun(K, V, A) ->
                            V2 = [P || P <- V, is_process_alive(P)],
                            [{K,V2}|A]
                    end, [], Subs),
    {noreply, State#state{subscriptions=dict:from_list(NewSubsList)}};

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

broadcast_message(Name, Message, State) ->
    case dict:find(Name, State#state.subscriptions) of
        error -> ok;
        {ok, Ps} ->
            Msg = {message, Name, Message, State#state.name},
            [P ! Msg || P <- Ps]
    end,
    State.


kill_sim(Name, IP, Port) ->
    Key = {sup, IP, Port},
    SimSup = ets:lookup_element(Name, sim_sup, 2),
    supervisor:terminate_child(SimSup, Key),
    supervisor:delete_child(SimSup, Key),
    ets:match_delete(Name, {{'_', IP, Port}, '_'}).


simconnect_from_login(State, Info) ->
    I = fun(K) -> ?GV(K, Info) end,
    AgentID = slerl_util:parse_uuid(I("agent_id")),
    SessionID = slerl_util:parse_uuid(I("session_id")),

    SimInfo = #sim{
      ip=slerl_util:parse_ip(I("sim_ip")),
      port=list_to_integer(I("sim_port")),
      circuitCode=list_to_integer(I("circuit_code")),
      regionPos={I("region_x"), I("region_y"), 0}, % Zero?
      seedCapability=I("seed_capability"),
      agentID=AgentID,
      sessionID=SessionID},

    slerl_sim_sup:start_sim_group(State#state.name, SimInfo).


simconnect_from_event(State, Event, Position) ->
    I = fun(K) -> ?GV(K, Event) end,
    AgentID = slerl_util:parse_uuid(I('AgentID')),

    % These don't change, apparently
    CircuitCode = list_to_integer(?GV("circuit_code", State#state.info)),
    SessionID = slerl_util:parse_uuid(?GV("session_id", State#state.info)),

    IP = list_to_tuple(I('SimIP')),
    Port = I('SimPort'),

    % Make sure we have a clean slate
    kill_sim(State#state.name, IP, Port),

    SimInfo = #sim{
      ip=IP,
      port=Port,
      circuitCode=CircuitCode,
      regionPos=Position,
      seedCapability=I('SeedCapability'),
      agentID=AgentID,
      sessionID=SessionID},

    slerl_sim_sup:start_sim_group(State#state.name, SimInfo).



current_sim(State) ->
    Key = State#state.currentSimKey,
    case ets:lookup(State#state.name, Key) of
        [{Key, Sim}] -> Sim;
        [] -> none
    end.


do_logout(State) ->
    ?DBG(logging_out),
    Sim = current_sim(State),
    case Sim of
        none -> exit(State#state.sup, shutdown);
        _ ->
            gen_server:cast(Sim, logout),
            timer:send_after(?LOGOUT_TIMEOUT, logout_timeout)
    end.


region_getter(Name, State) ->
    Sim = current_sim(State),
    SimInfo = State#state.simInfo,
    gen_server:call(Sim, {subscribe, 'MapBlockReply'}),
    Message = slerl_message:build_message(
                'MapNameRequest', [ [SimInfo#sim.agentID, SimInfo#sim.sessionID, 0, 0, false],
                                    [Name] ]),
    gen_server:cast(Sim, {send, Message, true}),
    receive
        {message, 'MapBlockReply', Response, _Conn} ->
             case [B || B <- ?GV('Data', Response#message.message),
                       ?GV('Name', B) == Name] of
                [Block] -> {ok, Block};
                [] -> {error, not_found};
                Other -> {error, {multiple_results, Other}}
            end;
        _ -> ignore
    after 10000 ->
            fail
    end.



%% handle_cast({map_block, Block}, State) ->
%%     Name = slerl_util:extract_string(?GV('Name', Block)),
%%     ?DBG({now_in, Name}),
%%     NewSimInfo = (State#state.simInfo)#sim{name=Name},
%%     {noreply, State#state{blockInfo=Block, simInfo=NewSimInfo}};

%% map_block_request(Handle, State) ->
%%     Sim = current_sim(State),
%%     SimInfo = State#state.simInfo,
%%     <<X:1/unsigned-integer-unit:32, Y:1/unsigned-integer-unit:32>> = <<Handle:1/unsigned-integer-unit:64>>,
%%     XR = round(X / 256), YR = round(Y / 256),
%%     gen_server:call(Sim, {subscribe, 'MapBlockReply'}),
%%     Message = slerl_message:build_message(
%%                 'MapBlockRequest',
%%                 [ [SimInfo#sim.agentID, SimInfo#sim.sessionID, 0, 0, false],
%%                   [ XR, XR, YR, YR ] ]),
%%     gen_server:cast(Sim, {send, Message, true}),
%%     receive
%%         {message, 'MapBlockReply', Response, _Conn} ->
%%             [Block] = ?GV('Data', Response#message.message),
%%             gen_server:cast(State#state.name, {map_block, Block});
%%         _ -> ignore
%%     after 10000 ->
%%             fail
%%     end.



%%--------------------------------------------------------------------
%%% Teleport
%%--------------------------------------------------------------------

do_teleport(From, RegionName, Position, State) ->
    Reply = case teleporter(RegionName, Position, State) of
                {ok, {remote, Info}} ->
                    simconnect_from_event(State, Info, Position),
                    {remote, pending};
                Other ->
                    Other
            end,
    gen_server:reply(From, Reply).


teleporter(RegionName, Position, State) ->
    Sim = current_sim(State),
    SimInfo = State#state.simInfo,
    case region_getter(RegionName, State) of
        {ok, Block} ->
            Handle = (((?GV('X', Block)*256) bsl 32) + (?GV('Y', Block)*256)),
            gen_server:call(Sim, {subscribe,
                                  ['TeleportStart', 'TeleportProgress',
                                   'TeleportFailed', 'TeleportFinish',
                                   'TeleportCancel', 'TeleportLocal']}),
            Message = slerl_message:build_message(
                        'TeleportLocationRequest',
                        [ [ SimInfo#sim.agentID, SimInfo#sim.sessionID ],
                          [ Handle, Position, {0, 1, 0} ] ]),
            gen_server:cast(Sim, {send, Message, true}),
            teleport_status_loop(Position);
        Other -> Other
    end.

teleport_status_loop(Position) ->
    receive
        {message, 'TeleportFailed', Msg, _Conn} ->
            [Info] = ?GV('Info', Msg#message.message),
            {failed, ?GV('Reason', Info)};
        {message, 'TeleportLocal', _Msg, _Conn} ->
            {ok, local};
        {message, 'TeleportStart', _Msg, _Conn} ->
            teleport_status_loop(Position);
        {message, 'TeleportProgress', Msg, _Conn} ->
            String = slerl_util:extract_string(
                       slerl_util:get_field(['Info', 'Message'], Msg#message.message)),
            ?DBG({progress, String}),
            teleport_status_loop(Position);
        {message, 'TeleportFinish', Msg, _Conn} ->
            [Info] = ?GV('Info', Msg#message.message),
            {ok, {remote, Info}};
        {message, Other, Response, _Conn} ->
            ?DBG({other, Other, Response}),
            teleport_status_loop(Position)
    after 20000 ->
            {failed, timeout}
    end.


%%--------------------------------------------------------------------
%%% Instant Messages
%%--------------------------------------------------------------------

avatar_uuid_finder(Name, State) ->
    Sim = current_sim(State),
    SimInfo = State#state.simInfo,
    gen_server:call(Sim, {subscribe, 'AvatarPickerReply'}),
    QueryUUID = slerl_util:random_uuid(),
    Message = slerl_message:build_message(
                'AvatarPickerRequest', 
                [ [SimInfo#sim.agentID, SimInfo#sim.sessionID, QueryUUID],
                  [Name] ]),
    gen_server:cast(Sim, {send, Message, true}),
    receive
        {message, 'AvatarPickerReply', 
         #message{message=[{'AgentData', [_, {'QueryID', QueryUUID}]},
                           {'Data', AvatarInfos}]}, _Conn} ->
            
            {ok, format_avatar_uuid_reply(AvatarInfos, [])}
    after 10000 ->
            {error, timeout}
    end.


format_avatar_uuid_reply([], Buff) ->
    lists:reverse(Buff);
format_avatar_uuid_reply([A|Rest], Buff) ->
    case ?GV('AvatarID', A) of
        <<0:16/integer-unit:8>> -> 
            format_avatar_uuid_reply(Rest, Buff);
        UUID -> 
            First = slerl_util:get_binary_string(['FirstName'], A),
            Last = slerl_util:get_binary_string(['LastName'], A),
            Name = list_to_binary([First, $\s, Last]),
            format_avatar_uuid_reply(Rest, [{Name, UUID}|Buff])
    end.



send_im(UUID, GivenSessionID, Text, State) ->
    Sim = current_sim(State),
    SimInfo = State#state.simInfo,
    
    IMSessionID = construct_session_id(SimInfo#sim.agentID, UUID, GivenSessionID),
    FromName = list_to_binary(atom_to_list(State#state.name)),

    Message = slerl_message:build_message(
                'ImprovedInstantMessage',
                [ [SimInfo#sim.agentID, SimInfo#sim.sessionID],
                  [false,  % FromGroup
                   UUID,   % ToAgentID
                   0,      % ParentEstateID
                   SimInfo#sim.regionID,
                   {0, 0, 0},
                   1,      % Offline
                   0,      % Dialog
                   IMSessionID,
                   0,      % Timestamp
                   FromName,
                   Text,
                   <<>>       % BinaryBucket
                   ] ]),

    gen_server:cast(Sim, {send, Message, true}).


construct_session_id(AgentID, AgentID, none) -> AgentID;
construct_session_id(AgentID, AgentID, AgentID) -> AgentID;
construct_session_id(AgentID, TargetID, none) -> crypto:exor(AgentID, TargetID);
construct_session_id(_, _, IMSessionID) -> IMSessionID.
