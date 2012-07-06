-module(mmd_web_cache).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([get_file/1,get_file/2]).
-export([minify/1]).
-export([clear/0]).
-export([minifier/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("p6core/include/p6core.hrl").

-define(MINIFY_TIMEOUT,5000).
-define(MINIFIER,mmd_webcache_minifier).
-define(SERVER, ?MODULE).

-define(TABLE,mmd_web_cache).
-record(entry,{file,last_mod,raw,content}).
%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local,?SERVER},?MODULE, [], []).

clear() -> gen_server:call(?SERVER,clear).

get_file(File) -> get_file(File,minified).
get_file(File,Type) ->
    case p6file:lastMod(File) of
	{ok,Time} ->
	    case ets:lookup(?TABLE,File) of
		[E=#entry{last_mod=Time}] ->
%%		    ?ldebug("Returning: ~p / ~p: ~p/~p",[File,Type,size(E#entry.content),size(E#entry.raw)]),
		    case Type of
			raw -> {ok,E#entry.raw};
			_ -> {ok,E#entry.content}
		    end;
		[#entry{}] -> cache_miss(File,out_of_date);
		[] -> cache_miss(File,not_cached)
	    end;
	Other -> Other
    end.
	    
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

				     
init([]) ->
    ?TABLE = ets:new(?TABLE,[named_table,protected,{keypos,#entry.file}]),
    register(?MINIFIER,spawn_link(fun()->minifier() end)),
    {ok,nostate}.

handle_call(stats,_From,State) ->
    Result = 
	lists:map(fun(#entry{file=File,raw=Raw,content=Content}) ->
			  {File,size(Raw),size(Content)}
		  end,
		  ets:tab2list(?TABLE)),
    {reply,Result,State};
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
	    ets:insert(?TABLE,Entry);
	Other ->
	    ?lwarn("Failed to store: ~p, reason: ~p",[File,Other])
    end,
    {noreply,State};
handle_cast({update_min,File,Bin},State) ->
    case ets:update_element(?TABLE,File,{#entry.content,Bin}) of
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

cache_miss(File,Reason) ->
    ?ldebug("Cache miss (~p): ~p",[Reason,File]),
    store(File),
    file:read_file(File).

store(File) -> gen_server:cast(?SERVER,{store,File}).

load(File) ->
    case p6file:lastMod(File) of
	{ok,Time} ->
	    case file:read_file(File) of
		{ok,Bin} ->
		    MBin = Bin,
		    minify(File),
		    {ok,#entry{file=File,last_mod=Time,raw=Bin,content=MBin}};
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
    
    % case filename:extension(File) of
    % 	<<".js">> -> yui_minify(File);
    % 	<<".css">> -> yui_minify(File);
    % 	_ -> unsupported
    % end.

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
