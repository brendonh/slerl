%%%-------------------------------------------------------------------
%%% File    : slerl_event_queue.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : Caps Event Queue
%%%
%%% Created : 12 Feb 2010 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(slerl_event_queue).

-behaviour(gen_server).

-include("slerl.hrl").
-include("slerl_util.hrl").

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
  name,
  simInfo,
  url,
  reqID,
  lastAck,
  connKey
}).

-define(HTTP_HEADERS, [{"Accept", "*/*"}, {"User-Agent", "Erlang/slerl"}]).
-define(HTTP_OPTIONS, [{timeout, 30000}, {relaxed, true}]).


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
    ets:insert(Name, {{events, SimInfo#sim.ip, SimInfo#sim.port}, self()}),
    gen_server:cast(self(), go),

    {ok, URL} = dict:find('EventQueueGet', SimInfo#sim.caps),
    {ok, #state{name=Name, simInfo=SimInfo, url=URL, reqID=none, lastAck=null,
                connKey={udp, SimInfo#sim.ip, SimInfo#sim.port}}}.


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


handle_cast(go, State) ->
    ?DBG({event_queue, starting}),
    ReqID = start_poll(null, false, State),
    {noreply, State#state{reqID=ReqID}};
    
handle_cast(stop, State) ->
    stop(State),
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({http, {ReqID, Result}}, #state{reqID=ReqID}=State) ->
    case handle_result(Result, State) of
        {ok, NewAck, NewDone} -> 
            NewReqID = start_poll(NewAck, NewDone, State),
            {noreply, State#state{reqID=NewReqID, lastAck=NewAck}};
        shutdown ->
            ?DBG({event_queue, shutdown}),
            SimInfo = State#state.simInfo,
            ets:delete(State#state.name, {events, SimInfo#sim.ip, SimInfo#sim.port}),
            {stop, normal, State}
    end;
    
handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, State) ->
    stop(State),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

get_conn(#state{name=Name, connKey=ConnKey}) ->
    ets:lookup_element(Name, ConnKey, 2).


start_poll(Ack, Done, State) ->
    Message = {map, [{ack, Ack}, {done, Done}]},
    XML = slerl_llsd:encode_xml(Message),
    Request = {State#state.url, ?HTTP_HEADERS, "application/xml", XML},
    {ok, ReqID} = http:request(post, Request, ?HTTP_OPTIONS, [{sync, false}]),
    ReqID.


handle_result({error, timeout}, State) ->
    {ok, State#state.lastAck, false};
handle_result({{_,502,_}, _, _}, _State) ->
    {ok, null, false};
handle_result({{_,200,_}, _, Body}, State) ->
    S = binary_to_list(Body),
    M = slerl_llsd:decode_xml(S),
    Messages = [build_event(E) || E <- ?GV(events, M)],
    NewAck = ?GV(id, M),
    case Messages of
        [] -> ok;
        _ -> 
            Conn = get_conn(State),
            gen_server:cast(Conn, {dispatch, Messages})
    end,
    {ok, NewAck, false};
handle_result({{_,404,_}, _Headers, _Body}, _State) ->
    shutdown;
handle_result({{_,401,_}, _Headers, _Body}, _State) ->
    shutdown;
handle_result({{_,Code,_}, Headers, Body}, _State) ->
    ?DBG({unexpected_reply, Code, Headers, Body}),
    {ok, null, false}.

                     

build_event(LLSD) ->
    MsgName = list_to_atom(?GV(message, LLSD)),
    #message{spec=ets:lookup_element(slerl_messages, MsgName, 2),
             message=?GV(body, LLSD)}.



stop(State) ->
    ?DBG({event_queue, stopping}),
    case State#state.reqID of
        none -> ok;
        ReqID -> http:cancel_request(ReqID)
    end,
    SimInfo = State#state.simInfo,

    catch ets:delete(State#state.name, {events, SimInfo#sim.ip, SimInfo#sim.port}).
            
