%%%-------------------------------------------------------------------
%%% File    : slerl_uuid_db.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : Database of known UUIDs
%%%
%%% Created : 14 Feb 2010 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(slerl_uuid_db).

-include("slerl.hrl").
-include("slerl_util.hrl").

-export([startup/0, add/1, find/1, get_uuid/1, scrape/2]).


%%====================================================================
%% Setup
%%====================================================================

startup() ->
    Node = node(),
    case mnesia:create_schema([Node]) of
        ok -> ?DBG({mnesia, schema_created});
        {error, {Node, {already_exists, Node}}} -> ok
    end,
    
    mnesia:start(),
    
    case mnesia:create_table(uuid, 
                             [{disc_copies, [Node]},
                              {attributes, record_info(fields,uuid)}]) of
        {atomic, ok} -> ?DBG({mnesia, {table_created, uuid}});
        {aborted, {already_exists, uuid}} -> ok
    end,
    
    ok.


%%====================================================================
%% API
%%====================================================================

add(#uuid{}=UUID) ->
    mnesia:activity(transaction, fun maybe_add/1, [UUID]).

   

find(Key) ->
    case mnesia:activity(transaction, fun mnesia:read/2, [uuid, Key]) of
        [UUID] -> UUID;
        _ -> not_found
    end.

get_uuid(Key) ->
    case find(Key) of
        not_found -> not_found;
        #uuid{uuid=UUID} -> UUID
    end.


%%====================================================================
%% Internal transaction funcs
%%====================================================================
             
maybe_add(#uuid{key=Key}=UUID) ->
    % Avoid hitting the disc if we can
    % Assumes UUIDS never change.
    case mnesia:read(uuid, Key) of
        [] -> mnesia:write(UUID);
        _ -> ok
    end.
             
        



%%====================================================================
%% Scraping
%%====================================================================

scrape('ChatFromSimulator', Message) -> 
    Data = ?GV('ChatData', Message),
    SourceID = ?GV('SourceID', Data),
    OwnerID = ?GV('OwnerID', Data),
    % Only chat from players, since objects have duplicate names
    if SourceID == OwnerID ->
            FromName = slerl_util:strip_zero(?GV('FromName', Data)),
            add(#uuid{key=FromName, uuid=SourceID});
       true -> ok
    end;
    
scrape('ImprovedInstantMessage', Message) ->
    AgentID = slerl_util:get_field(['AgentData', 'AgentID'], Message),
    % Is this always an actual agent? I'm not sure.
    FromName = slerl_util:get_binary_string(['MessageBlock', 'FromAgentName'], Message),
    add(#uuid{key=FromName, uuid=AgentID});

scrape('AvatarPickerReply', Message) ->
    scrape_picker_replies(?GV('Data', Message));

scrape(_, _) -> ok.


scrape_picker_replies([]) -> ok;
scrape_picker_replies([A|Rest]) ->
    case ?GV('AvatarID', A) of
        <<0:16/integer-unit:8>> -> ok;
        UUID -> 
            First = slerl_util:get_binary_string(['FirstName'], A),
            Last = slerl_util:get_binary_string(['LastName'], A),
            Name = list_to_binary([First, $\s, Last]),
            add(#uuid{key=Name, uuid=UUID})
    end,
    scrape_picker_replies(Rest).
    
