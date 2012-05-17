-module(mmd_cowboy_download).

%% used to allow a page to bounce content off the webserver to generate a download (basically, save a local file)

-include("mmd_cowboy_common.hrl").

-export([init/3, handle/2, terminate/2]).

init({_Proto,http}, Req, Cfg) ->
    {ok,Req,Cfg}.

terminate(_Req,_State) -> ok.

handle(OrigReq=#http_req{raw_path=Path,
    		urldecode={URLDecFun, URLDecArg}},Cfg) ->
%    io:fwrite("In handle,Path=~p",[Path]),    
    {ok, Data,_} = cowboy_http_req:body(OrigReq),     
    [{_, Text}] = cowboy_http:x_www_form_urlencoded(Data, fun(Bin) -> URLDecFun(Bin, URLDecArg) end),
    Headers = [{<<"Content-Type">>,mimetypes:filename(Path)}],
    {ok,NewReq} = cowboy_http_req:reply(200,Headers,Text,OrigReq),
    
    {ok,NewReq,Cfg}.
    
