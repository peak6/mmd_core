-module(mmd_cowboy_download).

%% used to allow a page to bounce content off the webserver to generate a download (basically, save a local file)

-include("mmd_cowboy_common.hrl").

-export([init/3, handle/2, terminate/2]).

init({_Proto,http}, Req, Cfg) ->
    {ok,Req,Cfg}.

terminate(_Req,_State) -> ok.

handle(OrigReq,Cfg) ->
    {Props,Req} = cowboy_http_req:qs_vals(OrigReq),
    {Path,_} = ?get(raw_path,OrigReq),
    Text = p6props:get(<<"data">>,Props),
    Headers = [{<<"Content-Type">>,mimetypes:filename(Path)}],
    {ok,NewReq} = cowboy_http_req:reply(200,Headers,Text,Req),
    
    {ok,NewReq,Cfg}.
    
