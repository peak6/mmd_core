-module(mmd_web_cache).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([get_file/1]).
-export([minify/1]).
-export([clear/0]).
-export([minifier/0]).
-export([scan/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("p6core/include/p6core.hrl").
-include("mmd_web_cache.hrl").

-define(MINIFY_TIMEOUT,5000).
-define(MINIFIER,mmd_webcache_minifier).
-define(SERVER, ?MODULE).

-define(TABLE,mmd_web_cache).
%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local,?SERVER},?MODULE, [], []).

clear() -> gen_server:call(?SERVER,clear).


get_file(File) -> 
    case ets:lookup(?TABLE,File) of
	[E=#cache_entry{}] -> {ok,E};
	[] -> cache_miss(File,not_cached)
    end.
	    
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

				     
init([]) ->
    ?TABLE = ets:new(?TABLE,[named_table,protected,{keypos,#cache_entry.file}]),
    register(?MINIFIER,spawn_link(fun()->minifier() end)),
    Scanner = spawn_link(fun scan/0),
    ?ldebug("Started scanner: ~p",[Scanner]),
    {ok,nostate}.

handle_call(clear,_From,State) ->
    ets:delete_all_objects(?TABLE),
    ?linfo("Clearing web cache"),
    {reply,ok,State};

handle_call(Request, From, State) ->
    ?linfo("Unexpected handle_call(~p, ~p, ~p)",[Request,From,State]),
    {reply, ok, State}.

handle_cast({store,File},State) ->
    case load(File) of
	{ok,Entry} ->
	    ets:insert(?TABLE,Entry),
            minify(File);
	Other ->
            ets:delete(?TABLE,File),
	    ?lwarn("Failed to store: ~p, reason: ~p",[File,Other])
    end,
    {noreply,State};
handle_cast({update_min,File,Bin},State) ->
    case ets:update_element(?TABLE,File,{#cache_entry.content,Bin}) of
	true -> ?ldebug("Minify stored: ~p, minified size: ~p",[File,size(Bin)]);
	false -> ?ldebug("No record matching: ~p",[File])
    end,
    {noreply,State}.
    
handle_info(Info, State) ->
    ?linfo("Unexpected handle_info(~p, ~p)",[Info,State]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

changed(File,OrigTime,NewTime) ->
    ?ldebug("Change detected: ~p, was: ~p, is: ~p",[File,OrigTime,NewTime]),
    store(File).
                                       
cache_miss(File,Reason) ->
    ?ldebug("Cache miss (~p): ~p",[Reason,File]),
    store(File),
    load(File).

store(File) -> gen_server:cast(?SERVER,{store,File}).

scan() ->
    timer:sleep(100),
    ets:foldl(fun(#cache_entry{file=File,mod_local=OrigTime},_) ->
                      case p6file:lastMod(File) of
                          {ok,OrigTime} -> ok;
                          {ok,OtherTime} -> changed(File,OrigTime,OtherTime);
                          Other -> changed(File,OrigTime,Other)
                      end
              end,
              ignored,
              ?TABLE),
    ?MODULE:scan().

load(File) ->
    case p6file:lastMod(File) of
	{ok,Time} ->
	    case file:read_file(File) of
		{ok,Bin} ->
                    Type = mimetypes:filename(File),
                    GMT = cowboy_clock:rfc2109(Time),
		    {ok,#cache_entry{file=File,mod_local=Time,type=Type,mod_gmtstr=GMT,content=Bin}};
		Other -> Other
	    end;
	Other -> Other
    end.

minify(File) when is_list(File) -> minify(p6str:mkbin(File));
minify(File) ->
    case filename:extension(File) of
	<<".js">> -> ?MINIFIER ! {yui,File};
	<<".css">> -> ?MINIFIER ! {yui,File};
	_ -> unsupported
    end.

minifier() ->
    receive
	{yui,File} ->
	    case yui_minify(File) of
		{ok,Bin} -> update_min(File,Bin);
		{error,{Code,Text}} ->
		    ?lwarn("Failed to minify: ~p, exit code: ~p, error message:~n~s",[File,Code,Text]);
		Other ->
		    ?ldebug("Error: ~p",[Other])
	    end;
	Other ->
	    ?lwarn("Unexpected message: ~p",[Other])
    end,
    ?MODULE:minifier().

update_min(File,Bin) -> gen_server:cast(?SERVER,{update_min,File,Bin}).

yui_minify(File) ->
    case p6exec:execArgs(?MINIFY_TIMEOUT,"java",["-jar","apps/mmd_core/priv/yuicompressor-2.4.7.jar",
						 "--preserve-semi",
						 "--disable-optimizations",
						 File]) of
	{0,Bin} -> {ok,Bin};
	{N,Bin} -> {error,{N,Bin}}
    end.


%% vim: ts=4:sts=4:sw=4:et:sta:
