-include_lib("p6core/include/p6core.hrl").
-include("mmd.hrl").
%%-include("mmd_http.hrl").
-include_lib("cowboy/include/http.hrl").
-include("mmd_cfg.hrl").

-record(htcfg,{port,root,trace=false,proxy,mmdCfg=#mmd_cfg{}}).

-define(XHR_DMAP,xhr_poll).

-define(get(Key,Req), cowboy_http_req:Key(Req)).
-define(trace(Cfg,Msg,Args), case Cfg#htcfg.trace of
				 true -> ?ldebug(Msg,Args);
				 _ -> ok
			     end).

