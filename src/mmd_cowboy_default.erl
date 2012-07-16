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
-module(mmd_cowboy_default).
-export([init/3, handle/2, terminate/2]).
-include("mmd_cowboy_common.hrl").
-include_lib("kernel/include/inet.hrl").
-define(trc(Flags,Fmt,Args), case mmd_web_flags:has(trace,Flags) of true -> ?ldebug(Fmt,Args); _ -> ok end).

init({_Proto,http}, Req, Cfg) ->
    {ok,Req,Cfg}.

handle(OrigReq,Cfg) ->
    {Path,Req} = ?get(raw_path,OrigReq),
    case Path of
	<<"/_hostname">> -> ok_txt(p6str:mkbin(get_hostname(Req)),Req,Cfg);
	<<"/_p6init.js">> -> ok_js([<<"var _p6MMDVars = ">>,mk_init(Cfg,Req)],Req,Cfg);
        <<"/wsurl">> -> ok_txt(get_wsurl(Req,Cfg),Req,Cfg);
        <<"/wsurl.js">> -> ok_js([<<"var p6MMDVars = ">>,mk_init(Cfg,Req)],Req,Cfg);
        _ -> handle_fs(Req,Cfg)
    end.

terminate(_Req,_State) -> ok.

redirect(To,Req,Cfg) ->
    {QS,_} = ?get(raw_qs,Req),
    case QS of
        <<>> -> ToLocation = To;
        _ -> ToLocation = [To,$?,QS]
    end,
    reply(307,[{<<"Location">>,[ToLocation]}],[<<"Redirect: ">>,ToLocation],Req,Cfg).

handle_error({error,enoent},Req,Cfg) -> not_found(Req,Cfg);
handle_error({error,eaccess}, Req, Cfg) -> reply(401,[],"Access denied",Req,Cfg);
handle_error(Other,Req,Cfg) -> reply(500,[],p6str:mkio("Unexpected error: ~p",[Other]),Req,Cfg).

handle_fs(Req,Cfg=#htcfg{root=_Root}) ->
    Path = case ?get(path,Req) of
	       {[],_} -> [<<"index.html">>];
	       {P,_} -> P
	   end,
    F = p6str:mkbin("~s/~s",[_Root,filename:join(Path)]),
    case p6file:fileType(F) of
	dir -> handle_dir(F,Req,Cfg);
	file -> send_file(F,Req,Cfg);
	Other -> handle_error(Other,Req,Cfg)
    end.

handle_dir(Path,Req,Cfg) ->
    {RawPath,_Req} = ?get(raw_path,Req),
    case binary:last(RawPath) of
	$/ -> send_file(filename:join([Path,<<"index.html">>]),Req,Cfg);
	_ -> redirect([RawPath,<<"/">>],Req,Cfg)
    end.

%% Used for sending raw files, skipping the cache
send_from_fs(Flags,File,Req,Cfg) ->
    ?trc(Flags,"Sending directly from filesystem: ~p",[File]),
    case file:read_file(File) of
        {ok,Bin} -> reply(200,
                          [{<<"Content-Type">>,mimetypes:filename(File)}],
                          Bin,
                          Req,
                          Cfg);
        Other -> handle_error(Other,Req,Cfg)
    end.

send_from_cache(Flags,#cache_entry{file=File,mod_gmtstr=CacheTime,content=Bin,type=Type},Req,Cfg) ->
    case mmd_web_flags:has(always,Flags) of
        true -> reply(200,[{<<"Content-Type">>,Type}],Bin,Req,Cfg);
        false ->
            case ?hdr('If-Modified-Since',Req) of
                {CacheTime,_} -> 
                    ?trc(Flags,"Not modified, sending 304: ~p",[File]),
                    reply(304,[],<<>>,Req,Cfg);
                {Other,_} -> 
                    ?trc(Flags,"Sending update: ~p, server time: ~p, browser time: ~p",[File,CacheTime,Other]),
                    reply(200,[
                               {<<"Content-Type">>,Type},
                               {<<"Last-Modified">>,CacheTime}
                              ],Bin,Req,Cfg)
            end
    end.

send_file(File,Req,Cfg) ->
    {Flags,_} = cowboy_http_req:cookie(<<"mmd">>,Req,<<>>),
    case mmd_web_flags:has(raw,Flags) of
        true -> 
            send_from_fs(Flags,File,Req,Cfg);
        false ->
            case mmd_web_cache:get_file(File) of
                {ok,Entry=#cache_entry{}} -> send_from_cache(Flags,Entry,Req,Cfg);
                Other -> handle_error(Other,Req,Cfg)
            end
    end.

get_wsurl(Req,#htcfg{port=Port}) ->
    Host = 
	case ?get(raw_host,Req) of %% Host client specified
	    {<<>>,_} ->
		case application:get_env(websocket_hostname) of %% config default
		    undefined ->
			{ok, H} = inet:gethostname(), %% Who I think I am
			H;
		    {ok, H} ->
			H
		end;
	    {Name,_} -> Name
	end,
    io_lib:format("ws://~s:~p/_ws",[Host,Port]).


not_found(Req,Cfg) ->
    {Raw,_} = ?get(raw_path,Req),
    reply(404,[],p6str:mkbin("File not found: ~s",[Raw]),Req,Cfg).

ok_js(Body,Req,Cfg)-> reply(200,[{<<"Content-Type">>,<<"application/javascript">>}],Body,Req,Cfg).
ok_txt(Body,Req,Cfg) -> reply(200,[{<<"Content-Type">>,<<"text/plain">>}],Body,Req,Cfg).

reply(RC,Headers,Body,Req,Cfg) ->
    {ok,NewReq} = cowboy_http_req:reply(RC,Headers,Body,Req),
    {ok,NewReq,Cfg}.

get_hostname(Req) ->
    {IP,_} = ?get(peer_addr,Req),
    case inet:gethostbyaddr(IP) of
	{ok,#hostent{h_name=undefined}} -> ?lwarn("Failed to resolve name for: ~p",[IP]),
					   p6str:ip_to_str(IP);
	{ok,#hostent{h_name=Name}} -> Name;
	Other -> ?lwarn("Failed to resolve hostname for: ~p, result: ~p",[IP,Other]),
		 p6str:ip_to_str(IP)
    end.

%% Returns content of a file or an empty binary, used for optional includes from filesystem
get_content(Path) ->
    okget:getOrElse(file:read_file(Path),<<"">>).

mk_init(Cfg=#htcfg{root=Root},Req) ->
    [
     <<"{\n \"websocketURL\": \"">>,get_wsurl(Req,Cfg),
     <<"\",\n \"client\": \"">>,p6str:mkbin(get_hostname(Req)),
     <<"\",\n \"environment\": \"">>,p6str:mkbin(p6init:getEnv()),
     <<"\",\n \"components\": {">>, gen_components(Cfg),
     <<" }\n};\n">>,get_content([Root,"/p6init.js"])
    ].

get_obj(Base) ->
    FileName = filename:join(Base,"component.json"),
    case file:read_file(FileName) of
	{ok,Bin} -> {FileName,Bin};
	_ -> undefined
    end.


%% Iterate webroot files looking for symlinks and generate js objects that represent those links
gen_components(#htcfg{root=Root}) ->
    Comps =
	lists:foldl( %% Each file
	  fun(Entry,Objs) ->
		  FullName = filename:join(Root,Entry),
		  case file:read_link(filename:join(Root,Entry)) of
		      {ok,OrigPath} -> %% Is symlink
			  Path = ensure_trailing_slash(OrigPath),
			  Ent =
			      case get_obj(FullName) of %% check for component.json
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

ensure_trailing_slash(Path) when is_list(Path) ->
    case lists:last(Path) of
	$/ -> Path;
	_ -> lists:append(Path,"/")
    end;
ensure_trailing_slash(Path) ->
    case binary:last(Path) of
	$/ -> Path;
	_ -> [Path,<<"/">>]
    end.

