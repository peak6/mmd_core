-module(mmd_cowboy_call).

-include("mmd_cowboy_common.hrl").

-export([init/3, handle/2, terminate/2]).

init({_Proto,http}, Req, Cfg) ->
    {ok,Req,Cfg}.

handle(OrigReq,Cfg) ->
    %%    ?trace(Cfg,"Request: ~p",[?DUMP_REC(http_req,OrigReq)]),
    {Props,Req} = cowboy_http_req:qs_vals(OrigReq),
    {PathInfo,_} = cowboy_http_req:path_info(Req),
    ?ldebug("PI: ~p",[PathInfo]),
    case PathInfo of 
	[Svc] -> handle(p6str:to_lower_bin(Svc),<<"application/json">>,Props,Req,Cfg);
	[Svc,File] -> handle(p6str:to_lower_bin(Svc),mimetypes:filename(File),Props,Req,Cfg);
	_ -> reply(404,[],"Bad request, must supply only a service and optionally a file name.  Example: /call/foo/bar.json",Req,Cfg)
    end.

handle(Svc,MimeType,Props,Req,Cfg) ->
    CC = mkCreateChannel(Svc,Props),
    case p6props:has(<<"debug">>,Props) of
        true ->
            CCStr = case CC of
                        #channel_create{} -> ?DUMP_REC(channel_create,CC);
                        Other -> Other
                    end,
	    {Path,_} = ?get(raw_path,Req),
	    {Args,_} = ?get(raw_qs,Req),

	    reply(200,[],p6str:mkio(
			   "URI:~p\n"
			   "Args: ~p\n"
			   "Using Args: ~p\n"
			   "Service: ~p\n"
			   "ChannelCreate: ~p"
				   ,[Path,Args,Props,Svc,CCStr]),Req,Cfg);
        false ->
            case CC of
                #channel_create{} ->
                    CallResult = mmd_call:call(CC),
		    ?trace(Cfg,"Call Results\n  Call: ~p\nResult: ~p",[?DUMP_REC(channel_create,CC),CallResult]),
                    case CallResult of
                        {ok,?error(?SERVICE_NOT_FOUND,Text)} ->
			    reply(404,[],p6str:mkio(Text),Req,Cfg);
                        {ok,?error(Code,Text)} ->
			    reply(500,[],p6str:mkio("Error Code: ~p\nError Message: ~s",[Code,p6str:mkstr(Text)]),Req,Cfg);
                        {ok,Result} ->
                            case json_encode:encode(Result) of
                                {ok,JSON} -> reply(200,[{<<"Content-Type">>,MimeType}],JSON,Req,Cfg);
				Other -> reply(500,[],p6str:mkio("Failed to encode response: ~p",[Other]),Req,Cfg)
                            end;
                        {error,timeout} -> reply(408,[],"Timeout",Req,Cfg);
                        Other -> reply(500,[],p6str:mkio("Call Error: ~p",[Other]),Req,Cfg)
                    end;
                Other -> reply(400,[],p6str:mkio("Bad Request: ~p",[Other]),Req,Cfg)
            end
    end.

terminate(_Req,_State) -> ok.

mkCreateChannel(Svc,Props) ->
    Vals =
        p6props:anyAndMap([{<<"token">>,fun p6uuid:safeparse/1},
                           {<<"body">>,fun json_decode:decodeObj/1},
                           {<<"timeout">>,{fun p6str:to_integer/1,5000}}],
                          Props),
    case Vals of
        [{error,_},_,_] -> {error,invalid_auth_token};
        [_,{error,_},_] -> {error,invalid_body};
        [undefined,undefined,Timeout] -> #channel_create{service=Svc,timeout=Timeout,type=call};
        [undefined,Body,Timeout] -> #channel_create{service=Svc,body=Body,timeout=Timeout,type=call};
        [Tok,undefined,Timeout] -> #channel_create{service=Svc,auth_token=Tok,timeout=Timeout,type=call};
        [Tok,Body,Timeout] -> #channel_create{service=Svc,auth_token=Tok,body=Body,timeout=Timeout,type=call}
    end.

reply(RC,Headers,Body,Req,Cfg) ->
    {ok,NewReq} = cowboy_http_req:reply(RC,Headers,Body,Req),
    {ok,NewReq,Cfg}.
