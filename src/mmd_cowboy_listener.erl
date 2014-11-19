-module(mmd_cowboy_listener).

-include("mmd_cowboy_common.hrl").
-export([start_link/1]).


-define(megs(N), p6mem:convert(N,mb,bytes)).

start_link(Port) ->
    application:start(mimetypes),
    application:start(cowboy),
    Root =
	case application:get_env(http_docroot) of
	    {ok,R} when is_list(R) -> R;
	    undefined -> os:getenv("HOME") ++ "/webroot"
	end,

    Proxy =
        case application:get_env(http_proxy_url) of
            undefined -> undefined;
            {ok,Val} -> p6str:mkstr(Val)
        end,
    Cfg = #htcfg{port=Port,root=p6str:mkbin(Root),proxy=Proxy},

    Dispatch = cowboy_router:compile([
									  {'_', [
											 {[<<"/_ws">>], mmd_cowboy_ws, Cfg},
											 {[<<"/call/:service[/:file]">>], mmd_cowboy_call,Cfg},
											 {[<<"/_call/:service[/:file]">>], mmd_cowboy_call,Cfg},
											 {[<<"/_mmd_flags.js">>], mmd_web_flags,Cfg},
											 {'_', mmd_cowboy_default, Cfg}
											]
									  }
									 ]),
    ?linfo("Starting http server on port: ~p with config: ~p",[Port,?DUMP_REC(htcfg,Cfg)]),
	cowboy:start_http(mmd_http_listener, 
					  100,  
					  [{port,Port},
					   {recbuf,?megs(1)},
					   {sndbuf,?megs(2)},
					   {ip,mmd_bind:ip()}],
					  [{env, [{dispatch,Dispatch}]},
					   {compress, true}
					  ]
					 ).
	
