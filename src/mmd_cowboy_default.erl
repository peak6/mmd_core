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
-define(WSLOOP(WS,Cfg,SESSION),?MODULE:wsLoop(WS,Cfg,SESSION)).
-export([init/3, handle/2, terminate/2]).
-include("mmd_cowboy_common.hrl").
-include_lib("kernel/include/inet.hrl").

init({_Proto,http}, Req, Cfg) ->
    {ok,Req,Cfg}.

handle(OrigReq,Cfg) ->
    {Path,Req} = ?get(raw_path,OrigReq),
    case Path of
	<<"/_hostname">> -> ok_txt(p6str:mkbin(get_hostname(Req)),Req,Cfg);
	<<"/_p6init.js">> -> ok_js([<<"var _p6MMDVars = ">>,mk_init(Cfg,Req)],Req,Cfg);
        <<"/wsurl">> -> ok_txt(get_wsurl(Cfg),Req,Cfg);
        <<"/wsurl.js">> -> ok_js([<<"var p6MMDVars = ">>,mk_init(Cfg,Req)],Req,Cfg);
        _ -> handle_fs(Req,Cfg)
    end.

terminate(_Req,_State) -> ok.

redirect(To,Req,Cfg) ->
    {QS,_} = ?get(raw_qs,Req),
    {OP,_} = ?get(raw_path,Req),
    case QS of
        <<>> -> From = OP,
                ToLocation = To;
        _ -> From = [OP,$?,QS],
             ToLocation = [To,$?,QS]
    end,
    ?trace(Cfg,"Redirecting: ~s -> ~s",[p6str:mkbin(From),p6str:mkbin(ToLocation)]),
    reply(307,[{<<"Location">>,[ToLocation]}],[<<"Redirect: ">>,ToLocation],Req,Cfg).

handle_error({error,enoent},Req,Cfg) -> not_found(Req,Cfg);
handle_error({error,eaccess}, Req, Cfg) -> reply(401,[],"Access denied",Req,Cfg);
handle_error(Other,Req,Cfg) -> reply(500,[],["Unexpected error: ~p",Other],Req,Cfg).

handle_fs(Req,Cfg=#htcfg{root=_Root}) ->
    {Path,Req} = ?get(path,Req),
    case resolve_links(Path,Req,Cfg) of
	{ok,dir,Dir} -> handle_dir(Dir,Req,Cfg);
	{ok,file,File} -> handle_file(File,Req,Cfg);
	{redirect,file,To} -> redirect(To,Req,Cfg);
	{redirect,dir,To} -> redirect(ensure_trailing_slash(To),Req,Cfg);
	Other ->
	    handle_error(Other,Req,Cfg)
    end.

resolve_links(Path,_Req,#htcfg{root=Root}) ->
    OrigFile = filename:join([Root|Path]),
    case p6file:fileType(OrigFile) of
	E = {error,_} -> E;
	Type ->
	    case p6file:real_path(Root,Path) of
		{ok,OrigFile} -> {ok,Type,OrigFile};
		{ok,NewFile} ->
		    case binary:split(NewFile,Root) of
			[<<>>,OrigFile] -> {ok,Type,OrigFile};
			[<<>>,Redir] -> {redirect,Type,Redir};
			[NewFile] -> {error,eaccess}
		    end;
		Other -> Other
	    end
    end.

handle_file(Path,Req,Cfg) ->
    send_file(Path,Req,Cfg).

handle_dir(Path,Req,Cfg) ->
    {RawPath,_Req} = ?get(raw_path,Req),
    case binary:last(RawPath) of
	$/ -> send_file(filename:join([Path,<<"index.html">>]),Req,Cfg);
	_ -> redirect([RawPath,<<"/">>],Req,Cfg)
    end.

send_file(File,Req,Cfg) ->
    ?trace(Cfg,"Sending: ~p",[File]),
    case file:read_file(File) of
	{ok,Bin} -> reply(200,[{<<"Content-Type">>,mimetypes:filename(File)}],Bin,Req,Cfg);
	Other -> handle_error(Other,Req,Cfg)
    end.


get_wsurl(#htcfg{port=Port}) ->
    Host = case application:get_env(websocket_hostname) of
	       undefined ->
		   {ok, H} = inet:gethostname(),
		   H;
	       {ok, H} ->
		   H
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
     <<"{\n \"websocketURL\": \"">>,get_wsurl(Cfg),
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

