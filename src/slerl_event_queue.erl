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
  reqID
}).

-define(HTTP_HEADERS, [{"Accept", "*/*"}, {"User-Agent", "Erlang/slerl"}]).
-define(HTTP_OPTIONS, [{relaxed, true}]).


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
    {ok, #state{name=Name, simInfo=SimInfo, url=URL, reqID=none}}.


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
    NewReqID = handle_result(Result, State),
    {noreply, State#state{reqID=NewReqID}};
    
handle_info(Info, State) ->
    ?DBG({unexpected_info, Info}),
    {noreply, State}.


terminate(_Reason, State) ->
    stop(State),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

start_poll(Ack, Done, State) ->
    Message = {map, [{ack, Ack}, {done, Done}]},
    XML = slerl_llsd:encode_xml(Message),
    Request = {State#state.url, ?HTTP_HEADERS, "application/xml", XML},
    {ok, ReqID} = http:request(post, Request, ?HTTP_OPTIONS, [{sync, false}]),
    ReqID.


handle_result({{_,502,_}, _, _}, State) ->
    start_poll(null, false, State);
handle_result({{_,200,_}, _, Body}, _State) ->
    ?DBG({got_body, Body}),
    none;
handle_result({{_,Code,_}, Headers, Body}, _State) ->
    ?DBG({unexpected_reply, Code, Headers, Body}),
    none.
                     


stop(State) ->
    ?DBG({event_queue, stopping}),
    case State#state.reqID of
        none -> ok;
        ReqID -> http:cancel_request(ReqID)
    end,
    SimInfo = State#state.simInfo,

    catch ets:delete(State#state.name, {events, SimInfo#sim.ip, SimInfo#sim.port}).
            
