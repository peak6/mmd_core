-include_lib("p6core/include/p6core.hrl").
-include("mmd.hrl").
%%-include("mmd_http.hrl").
-include_lib("cowboy/include/http.hrl").
-include("mmd_cfg.hrl").

-record(htcfg,{port,root,trace=false,proxy,mmdCfg=#mmd_cfg{}}).

-define(XHR_DMAP,xhr_poll).

-define(get(Key,Req), cowboy_http_req:Key(Req)).
-define(hdr(H,Req),cowboy_http_req:header(H,Req)).

-define(ok_js(Body,Req,Cfg), ?reply(200,[{<<"Content-Type">>,<<"application/javascript">>}],Body,Req,Cfg)).
-define(ok_txt(Body,Req,Cfg), ?reply(200,[{<<"Content-Type">>,<<"text/plain">>}],Body,Req,Cfg)).
-define(reply(RC,Headers,Body,Req,Cfg),{ok,NewReq} = cowboy_http_req:reply(RC,Headers,Body,Req),{ok,NewReq,Cfg}).


-define(trace(Cfg,Msg,Args), case Cfg#htcfg.trace of
				 true -> ?ldebug(Msg,Args);
				 _ -> ok
			     end).

