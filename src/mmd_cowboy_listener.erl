-module(mmd_cowboy_listener).

-include("mmd_cowboy_common.hrl").
-export([start_link/1]).


-define(megs(N), p6mem:convert(N,mb,bytes)).

start_link(Port) ->
    application:start(mimetypes),
    application:start(cowboy),
    p6dmap:new(?XHR_DMAP),
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

    Dispatch = [
		{'_', [
		       {[<<"_download">>,'...'], mmd_cowboy_download, Cfg},
		       {[<<"_ws">>], mmd_cowboy_ws, Cfg},
		       {[<<"call">>,'...'], mmd_cowboy_call,Cfg},
		       {[<<"_call">>,'...'], mmd_cowboy_call,Cfg},
		       {[<<"_xhr_poll">>,'...'], mmd_cowboy_xhr_poll,Cfg},
                       {[<<"_mmd_flags.js">>], mmd_web_flags,Cfg},
		       {'_', mmd_cowboy_default, Cfg}
		      ]
		}
	       ],
    ?linfo("Starting http server on port: ~p with config: ~p",[Port,?DUMP_REC(htcfg,Cfg)]),
    cowboy:start_listener(mmd_http_listener, 5,
			  cowboy_tcp_transport, [{port,Port},{recbuf,?megs(1)},{sndbuf,?megs(2)}],
			  cowboy_http_protocol, [{dispatch,Dispatch}]
			 ).
