%%%-------------------------------------------------------------------
%%% File    : slerl_caps.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : Capabilities
%%%
%%% Created : 11 Feb 2010 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(slerl_caps).

-export([request_seed_caps/1, post/2]).

-include("slerl.hrl").
-include("slerl_util.hrl").

-define(SEED_CAPS, ["ChatSessionRequest", "CopyInventoryFromNotecard", "DispatchRegionInfo", "EstateChangeInfo", "EventQueueGet", "FetchInventory", "WebFetchInventoryDescendents", "FetchLib", "FetchLibDescendents", "GroupProposalBallot", "HomeLocation", "MapLayer", "MapLayerGod", "NewFileAgentInventory", "ParcelPropertiesUpdate", "ParcelVoiceInfoRequest", "ProductInfoRequest", "ProvisionVoiceAccountRequest", "RemoteParcelRequest", "RequestTextureDownload", "SearchStatRequest", "SearchStatTracking", "SendPostcard", "SendUserReport", "SendUserReportWithScreenshot", "ServerReleaseNotes", "StartGroupProposal", "UntrustedSimulatorMessage", "UpdateAgentInformation", "UpdateAgentLanguage", "UpdateGestureAgentInventory", "UpdateNotecardAgentInventory", "UpdateScriptAgent", "UpdateGestureTaskInventory", "UpdateNotecardTaskInventory", "UpdateScriptTask", "UploadBakedTexture", "ViewerStartAuction", "ViewerStats"]).

-define(HTTP_HEADERS, [{"Accept", "*/*"},
                       {"User-Agent", "Erlang/slerl"}]).
-define(HTTP_OPTIONS, [{relaxed, true}]).
-define(REQ_OPTIONS, []).


request_seed_caps(URL) ->
    Content = [{string, C} || C <- ?SEED_CAPS],
    slerl_caps:post(URL, Content).



post(Cap, Content) ->
    XML = slerl_llsd:encode_xml(Content),
    Request = {Cap, ?HTTP_HEADERS, "application/llsd+xml", XML},
    case http:request(post, Request, ?HTTP_OPTIONS, ?REQ_OPTIONS) of
        {ok, {{_,200,_}, _Headers, Body}} ->
            try slerl_llsd:decode_xml(Body) of
                Caps when is_list(Caps) ->
                    {ok, Caps}
            catch Type:Error ->
                ?DBG(Body),
                ?DBG({decode_error, Type, Error}),
                {error, parse_error}
            end
    end.
