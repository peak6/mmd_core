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
        _ -> handle_fs(Path,Req,Cfg)
    end.

terminate(_Req,_State) -> ok.

redirect(OrigTo,Req,Cfg) ->
    To = re:replace(OrigTo,<<"/index\\.html">>,<<"/">>),
    {QS,_} = ?get(raw_qs,Req),
    case QS of
        <<>> -> ToLocation = To;
        _ -> ToLocation = [To,$?,QS]
    end,
    reply(307,[{<<"Location">>,[ToLocation]}],[<<"Redirect: ">>,ToLocation],Req,Cfg).

handle_error({error,enoent},Req,Cfg) -> not_found(Req,Cfg);
handle_error({error,eaccess}, Req, Cfg) -> reply(401,[],"Access denied",Req,Cfg);
handle_error(Other,Req,Cfg) -> reply(500,[],p6str:mkio("Unexpected error: ~p",[Other]),Req,Cfg).

handle_fs(<<$/>>,Req,Cfg) ->
    send_file(<<"/index.html">>,Req,Cfg); % url is /

handle_fs(RawPath,Req,Cfg=#htcfg{root=Root}) ->
    F = p6file:join(Root,RawPath),
    case p6file:fileType(F) of
	dir ->
            case binary:last(RawPath) of
                $/ -> send_file(p6file:join(RawPath,<<"index.html">>),Req,Cfg);%handle_dir(binary:part(RawPath,0,size(RawPath)-1),Req,Cfg);
                _ -> redirect(<<RawPath/binary,$/>>,Req,Cfg) % directories not ending in / must be redirected
            end;
	file -> send_file(RawPath,Req,Cfg);
	Other -> handle_error(Other,Req,Cfg)
    end.


send_file(RawPath,Req,Cfg=#htcfg{root=Root}) ->
    case p6str:ends_with(RawPath,<<".html">>) of
        true ->
            RSZ = size(Root),
            case p6file:real_path(Root,RawPath) of
                {ok,Full = <<Root:RSZ/binary,RawPath/binary>>} -> maybe_send(Full,Req,Cfg);
                {ok,<<Root:RSZ/binary,O/binary>>} -> redirect(O,Req,Cfg);%?ldebug("redirect: ~p",[O]);
                Err={error,_} -> handle_error(Err,Req,Cfg)
            end;
        false -> maybe_send(p6file:join(Root,RawPath),Req,Cfg)
    end.

maybe_send(File,Req,Cfg) ->
    {Flags,_} = cowboy_http_req:cookie(<<"mmd">>,Req,<<>>),
    send_from_fs(Flags,File,Req,Cfg).



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

get_myhost() ->
    {ok,Name} = inet:gethostname(),
    Name.

get_wsurl(_Req,#htcfg{port=Port}) ->
    io_lib:format("ws://~s:~p/_ws",[get_myhost(),Port]).


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

mk_init(Cfg=#htcfg{root=Root,port=Port},Req) ->
    [
     <<"{\n \"websocketURL\": \"">>,get_wsurl(Req,Cfg),
     <<"\",\n \"baseURL\": \"">>,p6str:mkbin("http://~s:~p",[get_myhost(),Port]),
     <<"\",\n \"datacenter\": \"">>,mmd:get_dc(),
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

