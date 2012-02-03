%% Copyright 2011 PEAK6 Investments, L.P.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
-module(http_handler).

-define(WSLOOP(WS,Cfg,SESSION),?MODULE:wsLoop(WS,Cfg,SESSION)).
-export([handleHttp/2, handleWs/2,wsLoop/3]).

-include_lib("p6core/include/p6core.hrl").
-include("mmd.hrl").
-include("mmd_http.hrl").

handleHttp(Cfg,Req) ->
    OrigPath =  Req:get(uri_unquoted),
    ?ldebug("PATH: ~p",[OrigPath]),
    case string:to_lower(OrigPath) of
	"/_p6init.js" -> Req:ok([{"Content-Type","text/javascript"}],[<<"var _p6MMDVars = ">>,mkInit(Cfg)]);
        "/_call/"++Svc -> doMMDCall(Req,p6str:mkatom(Svc));
        "/call/"++Svc -> doMMDCall(Req,p6str:mkatom(Svc));
        "/wsurl" -> Req:ok([{"Content-Type", "text/plain"}],getWSUrl(Cfg));
        "/wsurl.js" -> Req:ok([{"Content-Type","text/javascript"}],[<<"var p6MMDVars = ">>,mkInit(Cfg)]);
        _ -> checkAndSendFile(Cfg,Req,OrigPath)
    end.

%% Returns content of a file or an empty binary, used for optional includes from filesystem
getContent(Path) ->
    okget:getOrElse(file:read_file(Path),<<"">>).

mkInit(Cfg=#htcfg{root=Root}) ->
    [
     <<"{\n \"websocketURL\": \"">>,getWSUrl(Cfg),
     <<"\",\n \"environment\": \"">>,p6str:mkbin(p6init:getEnv()),
     <<"\",\n \"components\": {">>, genComponents(Cfg),
     <<" }\n};\n">>,getContent(Root++"/p6init.js")
    ].

getObj(Base) ->
    FileName = filename:join(Base,"component.json"),
    case file:read_file(FileName) of
	{ok,Bin} -> {FileName,Bin};
	_ -> undefined
    end.


%% Iterate webroot files looking for symlinks and generate js objects that represent those links
genComponents(#htcfg{root=Root}) ->
    Comps = 
	lists:foldl( %% Each file
	  fun(Entry,Objs) ->
		  FullName = filename:join(Root,Entry),
		  case file:read_link(filename:join(Root,Entry)) of 
		      {ok,OrigPath} -> %% Is symlink
			  Path = ensureTrailingSlash(OrigPath),
			  Ent = 
			      case getObj(FullName) of %% check for component.json
				  undefined -> [<<"\n  \"">>,Entry,<<"\": {\"path\": \"/">>,Path,<<"\"}">>];
				  {ObjFile,Bin} -> [<<"\n  \"">>,
						     Entry,<<"\": {\"path\": \"/">>,Path,
						     <<"\", \"obj\":\n//FILE: ">>,ObjFile,<<"\n">>,
						     Bin,<<"\n//END OF: ">>,ObjFile,<<"\n }">>]
			      end,
			  case Objs of
			      [] -> [Ent];
			      _ -> [Ent,<<",">>|Objs]
			  end;
		      _Ignore -> Objs
		  end
	  end,
	  [],
	  okget:ok(file:list_dir(Root))
	 ),
    lists:reverse(Comps).

    
handleWs(Ws,Cfg) ->
    con_tracker:registerConnection(self(),websocket,p6str:ip_port_to_str(okget:ok(Ws:get(peer_addr)),Ws:get(peer_port)),Ws:get(socket)),
    ?WSLOOP(Ws,Cfg,channel_mgr:new()).

addArgs(Req,Path) ->
    case Req:get(args) of
        [] -> Path;
        Args -> Path++"?"++Args
    end.

doProxy(Req,#htcfg{proxy=undefined},Url) ->
    ?lwarn("Missing requested file: ~p",[Url]),
    Req:respond(404,[],"Not Found: "++Req:get(uri_unquoted));
doProxy(Req,#htcfg{proxy=Proxy},Url) ->
    case httpc:request(addArgs(Req,Proxy++Url)) of
        {ok,{{_,200,_},Headers,Body}} -> Req:ok(Headers,Body);
        {ok,{R={_,RC,_},H,B}} -> ?linfo("Failed proxy call to: ~p, ~p",[Url,R]),
                                 Req:respond(RC,H,B)
    end.

checkAndSendFile(Cfg,Req,File) ->
    case lists:member(0,File) of
        true -> Req:respond(403,"Filename cannot contain null");
        false -> sendFile(Cfg,Req,File)
    end.

notFound(Req,Path) -> Req:respond(404,[],"Not Found: "++Path).

redirect(Req,To) ->
    RedirWithArgs = addArgs(Req,To),
    ?ldebug("Redirecting '~s' -> '~s'",[Req:get(uri_unquoted),RedirWithArgs]),
    Req:respond(307,[{"Location",RedirWithArgs}],"Redirect: "++RedirWithArgs).

ensureTrailingSlash(Path) ->
    case lists:reverse(Path) of
	[$/|_Rem] -> Path;
	Reversed -> lists:reverse([$/|Reversed])
    end.

shouldRedirect(Cfg,[$/|Rest]) -> shouldRedirect(Cfg,Rest);
shouldRedirect(#htcfg{root=Root},Path) ->
    case filename:split(Path) of
	[] -> {false,"/"};
	[Base|Rest] -> shouldRedirect(Root,Base,false,Rest)
    end.

shouldRedirect(Root,Base,Redirect,Rest) ->
    FullPath = filename:join(Root,Base),
    case file:read_link(FullPath) of
	{ok,Base} -> {false,filename:join([$/|Base],filename:join(Rest))}; %% self link should not redirect
	{ok,Link} ->
	    shouldRedirect(Root,Link,true,Rest);
	{error,_} -> %% einval / enoent both will redirect
	    case Rest of
		[] -> {Redirect,[$/|Base]};
		_NonEmpty -> {Redirect,filename:join([$/|Base],filename:join(Rest))}
	    end
    end.
    
endsWithSlash(Path) ->
    case lists:last(Path) of
	$/ -> true;
	_ -> false
    end.

sendFile(_Cfg,Req,"/favicon.ico") -> notFound(Req,"/favicon.ico");

sendFile(Cfg=#htcfg{root=Root},Req,OrigFile) ->
    case shouldRedirect(Cfg,OrigFile) of
	{true,To} ->
	    redirect(Req,To);
	{false,File} ->
	    FullPath = Root++File,
	    case p6file:fileType(FullPath) of
		{error,enoent} -> doProxy(Req,Cfg,File);
		{error,eloop} -> ?lerr("Detected link to self: ~p",[FullPath]),
				 notFound(Req,OrigFile);
		file -> misultin_req:file(FullPath,Req);
		dir ->
		    case endsWithSlash(OrigFile) of
			false -> redirect(Req,ensureTrailingSlash(OrigFile));
			true ->
			    Index = filename:join(FullPath,"index.html"),
			    case filelib:is_regular(Index) of
				true ->
				    misultin_req:file(Index,Req);
				false ->
				    notFound(Req,OrigFile)
			    end
		    end
	    end
    end.

mkCreateChannel(Svc,Props) ->
    Vals = 
        p6props:anyAndMap([{"token",fun p6uuid:safeparse/1},
                           {"body",fun json_decode:decodeObj/1},
                           {"timeout",{fun erlang:list_to_integer/1,5000}}],
                          Props),
    case Vals of
        [{error,_},_,_] -> {error,invalid_auth_token};
        [_,{error,_},_] -> {error,invalid_body};
        [undefined,undefined,Timeout] -> #channel_create{service=Svc,timeout=Timeout,type=call};
        [undefined,Body,Timeout] -> #channel_create{service=Svc,body=Body,timeout=Timeout,type=call};
        [Tok,undefined,Timeout] -> #channel_create{service=Svc,auth_token=Tok,timeout=Timeout,type=call};
        [Tok,Body,Timeout] -> #channel_create{service=Svc,auth_token=Tok,body=Body,timeout=Timeout,type=call}
    end.

doMMDCall(Req,Svc) ->
    Props = lists:map(fun({K,V}) -> {string:to_lower(K),V} end, Req:parse_qs()),
    CC = mkCreateChannel(Svc,Props),
    case p6props:has("debug",Props) of
        true ->
            CCStr = case CC of
                        #channel_create{} -> ?DUMP_REC(channel_create,CC);
                        Other -> Other
                    end,
            Req:ok([],
                   "URI:~p\n"
                   "Args: ~p\n"
                   "Using Args: ~p\n"
                   "Service: ~p\n"
                   "ChannelCreate: ~p"
                   ,[Req:get(uri),Req:get(args),Props,Svc,CCStr]);
        false ->
            case CC of
                #channel_create{} ->
                    CallResult = mmd_call:call(CC),
                    case CallResult of
                        {ok,?error(?SERVICE_NOT_FOUND,Text)} ->
                            Req:respond(404,[],p6str:mkstr(Text));
                        {ok,?error(Code,Text)} ->
                            Req:respond(500,[],p6str:mkstr("Error Code: ~p\nError Message: ~s",[Code,p6str:mkstr(Text)]));
                        {ok,Result} -> 
                            case json_encode:encode(Result) of
                                {ok,JSON} -> Req:ok([],JSON);
                                Other -> Req:respond(500,[],p6str:mkstr("Failed to encode response: ~p",[Other]))
                            end;
                        {error,timeout} -> Req:respond(408,[],"Timeout"); 
                        Other -> Req:respond(500,[],p6str:mkstr("Call Error: ~p",[Other])) 
                    end;
                Other -> Req:respond(400,[],p6str:mkstr("Bad Request: ~p",[Other]))
            end
    end.

getWSUrl(#htcfg{port=Port}) ->
    Host = case application:get_env(websocket_hostname) of
	       undefined ->
		   {ok, H} = inet:gethostname(),
		   H;
	       {ok, H} ->
		   H
	   end,
    io_lib:format("ws://~s:~p/service",[Host,Port]).

toBin(Term) -> erlang:iolist_to_binary(io_lib:format("~p",[Term])).

wsLoop(Ws,Cfg=#htcfg{trace=Trace},Sess) -> 
    receive
        {browser,Data} ->
            case Trace of 
                true -> ?ldebug("Received(~p): ~p",[length(Data),Data]);
                _ -> ok
            end,
            case catch json_decode:decode(Data) of
                {'EXIT',{Reason,Stack}} ->
                    ?lerr("Error: ~p / ~p, processing: ~p",[Reason,Stack,Data]),
                    Ws:send(json:encode({obj,[{'_sock_error',toBin({Reason,Stack})}]})), %%Special encode, not mmd
                    ?WSLOOP(Ws,Cfg,Sess);
                Err = {error,_} -> 
                    ?lwarn("Received unparsable json text, error: ~p -- text: ~p",[Err,Data]),
                    Ws:send(json:encode({obj,[{'_sock_error',toBin(Err)}]})), %%Special encode, not mmd
                    ?WSLOOP(Ws,Cfg,Sess);
                Msg -> 
                    {NewCfg,NewChans} = handleIn(Cfg,Ws,Msg,Sess),
                    ?WSLOOP(Ws,NewCfg,NewChans)
            end;
        cfg ->
            ?linfo("Config is: ~p",[Cfg]),
            ?WSLOOP(Ws,Cfg,Sess);
        trace -> 
            ?linfo("Enabled tracing"),
            ?WSLOOP(Ws,Cfg#htcfg{trace=true},Sess);
        notrace -> 
            ?linfo("Disabled tracing"),
            ?WSLOOP(Ws,Cfg#htcfg{trace=false},Sess);
        {'$gen_call',Ref,{mmd,From,Msg}} ->
            gen_server:reply(Ref,ok),
            case Trace of
                true -> ?linfo("Sending: ~p",[Msg]);
                _ -> ok
            end,
            ?WSLOOP(Ws,Cfg,handleOut(Ws,From,Msg,Sess));
        {'$gen_call',Ref,getChannels} -> 
            gen_server:reply(Ref,Sess), 
            ?WSLOOP(Ws,Cfg,Sess);
        Ignore ->
            ?lwarn("Ignoring unexpected message: ~p",[Ignore]),
            ?WSLOOP(Ws,Cfg,Sess)
    end.

fire(Ws,Msgs) when is_list(Msgs) -> 
    lists:foreach(fun(M) -> fire(Ws,M) end, Msgs);
fire(Ws,Msg) -> 
    case catch json_encode:encode(Msg) of
        {ok,JSON} -> Ws:send(JSON);
        Error -> 
            ?lerr("Error encoding: ~p~nError message: ~p",[Msg,Error]),
            Ws:send(mkError(Msg,"Error encoding response, contact support."))
    end.

mkError(#channel_create{id=Id},Msg) -> mkError(Id,Msg);
mkError(#channel_close{id=Id},Msg) -> mkError(Id,Msg);
mkError(#channel_message{id=Id},Msg) -> mkError(Id,Msg);
mkError(Id,Msg) -> mkError(Id,?SERVICE_ERROR,Msg).

mkError(Id,Code,Msg) ->
    json:encode(
      {obj,[
            {close,p6str:mkbin(uuid:to_string(Id))},
            {body,{obj,[{<<"_mmd_error">>,Code},{msg,p6str:mkbin(Msg)}]}}
           ]
      }).    

handleOut(Ws,From,Msg,Chans) ->
    case channel_mgr:processIn(Chans,From,Msg) of
        {NewChans,Dispatch} -> fire(Ws,Dispatch), NewChans;
        NewChans -> NewChans
    end.

handleIn(Cfg=#htcfg{mmdCfg=MMDCfg},Ws,#channel_create{id=Id,service='$mmd',body=?map(Map)}, Chans) ->
    NewMMDCfg = mmd_cfg:update(MMDCfg,lists:map(fun({A,B}) -> {p6str:mkatom(A),B} end,Map)),
    ?linfo("Updated MMDCfg: ~w -> ~w",[MMDCfg,NewMMDCfg]),
    fire(Ws,#channel_close{id=Id,body=ok}),
    {Cfg#htcfg{mmdCfg=NewMMDCfg},Chans};

handleIn(Cfg,Ws,Msg,Chans) ->
    case channel_mgr:processOut(Chans,Msg,Cfg#htcfg.mmdCfg) of
        {NewChans,ForMe} ->
            fire(Ws,ForMe),
            {Cfg,NewChans}
    end.

