-module(mmd_cowboy_xhr_poll).
-define(WSLOOP(WS,Cfg,SESSION),?MODULE:wsLoop(WS,Cfg,SESSION)).
-export([init/3, handle/2, terminate/2]).
-include("mmd_cowboy_common.hrl").

init({_Proto,http},Req,Cfg) ->
    {ok,Req,Cfg}.

handle(OReq,Cfg) ->
    {[_Me,Id],Req} = ?get(path,OReq),
    ?ldebug("Lookup: ~p",[Id]),
    case p6dmap:get(?XHR_DMAP,Id) of
	[] -> not_found(Id,Req,Cfg);
	[[Pid,_Ignored]] -> ?ldebug("Found: ~p",[Pid]),
			    case catch gen_server:call(Pid,next_response,infinity) of
				{text,Data} -> reply(200,[{<<"Content-Type">>,<<"application/json">>}],encode(Data,text),Req,Cfg);
				Other -> ?lerr("Not sure what to do with: ~p",[Other])
			    end
    end.

terminate(_Req,_Cfg) ->
    ok.

encode(Msg,binary) -> mmd_encode:encode(Msg);
encode(Msg,text) -> okget:ok(json_encode:encode(Msg)).

not_found(Id,Req,Cfg) ->
    reply(404,[],p6str:mkbin("ID Not Found: ~s",[Id]),Req,Cfg).

%%ok_js(Body,Req,Cfg)-> reply(200,[{<<"Content-Type">>,<<"application/javascript">>}],Body,Req,Cfg).
%%ok_txt(Body,Req,Cfg) -> reply(200,[{<<"Content-Type">>,<<"text/plain">>}],Body,Req,Cfg).

reply(RC,Headers,Body,Req,Cfg) ->
    {ok,NewReq} = cowboy_http_req:reply(RC,Headers,Body,Req),
    {ok,NewReq,Cfg}.
