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
-module(p6hot_deploy).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([enable/0,enable/1]).
-export([disable/0]).
-export([reload/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-include("logger.hrl").
-include_lib("kernel/include/file.hrl").
-record(state, {lastTime=stamp(1),interval,tracked=[],enabled=false}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

enable() ->
    gen_server:call(?MODULE,enable).

enable(Timeout) ->
    gen_server:call(?MODULE,{enable,Timeout}).

disable() ->
    gen_server:call(?MODULE,disable).

reload() ->
    gen_server:call(?MODULE,reload).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    Interval = case application:get_env(check_interval) of
		   undefined -> 1000;
		   {ok,Val} -> Val
	       end,
    State = #state{interval=Interval},
    case application:get_env(auto_start) of
	{ok,true} -> {ok,State#state{enabled=true},Interval};
	undefined -> {ok,State#state{enabled=true},Interval};
	_ -> {ok,State}
    end.


handle_call(enable, _From, State=#state{interval=Int}) ->
    ?linfo("Reload check re-enabled at: ~p ms",[Int]),
    mkReply(ok,State#state{enabled=true});
handle_call({enable,Interval}, _From,State) ->
    ?linfo("Reload check set to: ~p ms",[Interval]),
    mkReply(ok,State#state{interval=Interval,enabled=true});
handle_call(disable,_From,State) ->
    mkReply(ok,State#state{enabled=false});
handle_call(reload, _From, State) ->
    {NewTime,Reloaded} = doReload(State),
    mkReply({ok,Reloaded},State#state{lastTime=NewTime}).
mkReply(Body,State=#state{interval=Interval,enabled=true}) ->
    {reply,Body,State,Interval};
mkReply(Body,State=#state{}) ->
    {reply,Body,State}.

handle_cast(Msg, State) ->
    ?linfo("Unexpected handle_cast(~p, ~p, ~p)",[Msg,State]),
    mkNoReply(State).

handle_info(timeout,State=#state{tracked=Tracked}) ->
    case doReload(State) of
	{NewTs,[]} -> mkNoReply(State#state{lastTime=NewTs});
	{NewTs,Results} ->
            NewTracked = lists:foldl(fun({N,S},NT) -> stateChanged(N,S,NT) end, Tracked, Results),
            mkNoReply(State#state{lastTime=NewTs,tracked=NewTracked})
%%            ?linfo("Reload Results for ~p -> ~p: ~p",[dateStr(LT),dateStr(NewTs),Results])
    end;

handle_info(Info, State) ->
    ?linfo("Unexpected handle_info(~p, ~p)",[Info,State]),
    mkNoReply(State).

stateChanged(Name,{error,enotdir},Tracked) ->
    case lists:member(Name,Tracked) of
        true ->
            Tracked;
        false ->
            [Name|Tracked]
    end;
stateChanged('lager_mochiglobal:loglevel',missing,Tracked) -> Tracked;
stateChanged(Name,missing,Tracked) ->
    case lists:member(Name,Tracked) of
        true ->
            Tracked;
        false ->
            ?lwarn("Missing module: ~p",[Name]),
            [Name|Tracked]
    end;
stateChanged(Name,reloaded,Tracked) ->
    ?linfo("Reloaded: ~p",[Name]),
    lists:delete(Name,Tracked);
stateChanged(Name,State,Tracked) ->
    ?linfo("Module: ~p, ~p",[Name,State]),
    Tracked.

mkNoReply(State=#state{interval=Interval,enabled=true}) -> {noreply,State,Interval};
mkNoReply(State) -> {noreply,State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
doReload(#state{lastTime=When}) ->
    doReload(When,stamp()).

doReload(From,To) ->
    {To,lists:foldl(
	  fun({Module,File},Acc) when is_list(File) ->
		  case check(From,To,Module,File) of
		      unmodified -> Acc;
		      Other -> [{Module,Other}|Acc]
		  end;
	     (_,Acc) -> Acc
	  end, [], code:all_loaded())}.

    %% Results = lists:foldl(fun(M)
    %% 		fun({Module,File}) when is_list(File) ->
    %% 	 end || {Module,File} <- , is_list(File)]}.

check(From,To,Module,File) ->
    case file:read_file_info(File) of
	{ok,#file_info{mtime=MTime}} when MTime >= From, MTime < To ->
	    case reload(Module) of
		{module,Module} -> reloaded;
		Other -> Other
	    end;
	{ok,_} -> unmodified;
	{error,enoent} ->
	    missing;
	Other -> Other
    end.

reload(Module) ->
    case code:purge(Module) of
	true -> ?lwarn("Process killed as result of reloading: ~p",[Module]);
	false -> ok
    end,
    code:load_file(Module).

%%dateStr({{Y,M,D},{H,Min,S}}) -> lists:flatten(io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",[Y,M,D,H,Min,S])).

%% Current time, in the form of {{Year,Mon,Date},{Hour,Min,Sec}}
stamp() ->
    erlang:localtime().

%% Timestamp offset by N seconds
stamp(N) ->
    calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(stamp())+N).

