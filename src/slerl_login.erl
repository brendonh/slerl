%%%-------------------------------------------------------------------
%%% File    : slerl_login.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : Login
%%%
%%% Created :  6 Feb 2010 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(slerl_login).

-export([login/6]).
-export([test/0]).

-include_lib("xmerl/include/xmerl.hrl").

-define(HTTP_HEADERS, [{"Accept", "*/*"},
                       {"User-Agent", "Erlang/slerl"}]).
-define(HTTP_OPTIONS, [{relaxed, true}]).
-define(REQ_OPTIONS, []).

-define(GV(E, P), proplists:get_value(E, P)).
-define(GVD(E, P, D), proplists:get_value(E, P, D)).
-define(DEBUG(T), io:format("~p ~p~n", [self(), T])).
-define(DBGS(T), io:format("~p ~s~n", [self(), T])).


login(URL, First, Last, Password, {Major, Minor, Patch, Build}, Start) ->
    MD5Pass = [$$,$1,$$ | slerl_util:md5_hex(Password)],
    Members = [{first, First}, {last, Last}, {passwd, MD5Pass}, {start, Start},
               {major, Major}, {minor, Minor}, {patch, Patch}, {build, Build},
               {platform, "Lnx"}, {mac, slerl_util:macaddr()},
               {agree_to_tos, "true"}],
    MemberXML = lists:flatten([format_member(M) || M <- Members]),
    XML = wrap_xml(MemberXML),
    get_response(URL, XML).
        
       
format_member({K, V}) -> 
    lists:flatten(
      io_lib:format(
        "<member><name>~p</name><value>~s</value></member>", [K, V])).


wrap_xml(MemberXML) ->
    lists:flatten(
        ["<?xml version=\"1.0\"?>",
         "<methodCall><methodName>login_to_simulator</methodName>",
         "<params><param><value><struct>",
         MemberXML,
         "</struct></value></param></params></methodCall>"]).


get_response(URL, XML) ->
    Request = {URL, ?HTTP_HEADERS, "text/xml", XML},
    case http:request(post, Request, ?HTTP_OPTIONS, ?REQ_OPTIONS) of
        {ok, {{_,200,_}, _Headers, Body}} ->
            Response = parse_response(Body),
            case ?GV("login", Response) of
                "true" -> {ok, Response};
                Other -> {error, {login_failed, Response}}
            end;
        Other ->
            ?DEBUG({error, Other}),
            {error, {http_error, Other}}
    end.


parse_response(XML) ->
    {Parsed, _} = xmerl_scan:string(XML),
    [member_as_kv(M) || M <- xmerl_xpath:string("//member", Parsed)]. 

member_as_kv(M) ->
    [Key] = xmerl_xpath:string("name/text()", M),
    [Value|_] = xmerl_xpath:string("value/*/text()", M),
    {Key#xmlText.value, Value#xmlText.value}.


test() ->
    application:start(slerl),
    application:start(inets),
    application:start(ssl),

    {ok, [Bits]} = file:consult("login.config"),
    First = ?GV(first, Bits),
    Last = ?GV(last, Bits),
    Password = ?GV(pass, Bits),
    {ok, URL} = application:get_env(slerl, login_url),
    {ok, VersionInts} = application:get_env(slerl, client_version),
    Version = list_to_tuple([integer_to_list(I) || I <- tuple_to_list(VersionInts)]),
    Response = login(URL, First, Last, Password, Version, "last"),
    
    ?DEBUG(Response),

    application:stop(inets).
    %?DBG({First, Last, Password, URL}).
