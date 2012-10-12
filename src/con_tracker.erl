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
%%%-------------------------------------------------------------------
%%% @author dbudworth@peak6.com
%%% @copyright (C) 2011, PEAK6 Investments, L.P.
%%% @doc
%%%
%%% @end
%%% Created : Sun Apr 24 08:06:37 CDT 2011
%%%-------------------------------------------------------------------
-module(con_tracker).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([registerConnection/6]).
-export([getConnections/0]).
-export([clearConnections/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include_lib("p6core/include/logger.hrl").


-define(SERVER, ?MODULE).

-define(TABLE,mmd_cons).
-include("con_tracker.hrl").

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

registerConnection(Pid,Type,Name,Port,Impl,Vsn) ->
    call({register,Pid,Type,Name,Port,Impl,Vsn}).

getConnections() -> ets:tab2list(?TABLE).

clearConnections() -> call(clearAll).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ?TABLE = ets:new(?TABLE,[named_table,protected,{keypos,#con_info.ref}]),
    {ok, nostate}.

handle_call(clearAll,_From,State) ->
    lists:foreach(fun(#con_info{ref=Ref}) -> demonitor(Ref,[flush]) end, ets:tab2list(?TABLE)),
    ets:delete_all_objects(?TABLE),
    {reply,ok,State};

handle_call({register,Pid,Type,Name,Port,Impl,Vsn},_From,State) ->
    ?linfo("Connected ~p: ~s (~p/~p, \"~s\", ~s)",
           [Type,Name,Pid,Port,Impl,Vsn]),
    Ref = erlang:monitor(process,Pid),
    {reply,ets:insert_new(?TABLE,#con_info{ref=Ref,name=Name,pid=Pid,type=Type,port=Port,impl=Impl,vsn=Vsn}),State};

handle_call(Request, From, State) ->
    ?lwarn("Unexpected handle_call(~p, ~p, ~p)",[Request,From,State]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    ?lwarn("Unexpected handle_cast(~p, ~p)",[Msg,State]),
    {noreply, State}.

handle_info(Down={'DOWN',Ref,process,Pid,Reason},State) ->
    case ets:lookup(?TABLE,Ref) of
        [] -> ?linfo("Received unexpected DOWN message: ~p",[Down]);
        [#con_info{port=Port,impl=Impl,vsn=Vsn,name=Name,type=Type,start=Start}] -> ?linfo("Closed ~p: ~s (~p/~p, \"~s\", ~s), uptime: ~w, reason: ~p",[Type,Name,Pid,Port,Impl,Vsn,uptime(Start),Reason]);
        Other -> ?linfo("Unexpected response from ets lookup of ref: ~p, response was: ~p",[Ref,Other])
    end,
    ets:delete(?TABLE,Ref),
    {noreply,State};
handle_info(Info, State) ->
    ?lwarn("Unexpected handle_info(~p, ~p)",[Info,State]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
call(Term) -> gen_server:call(?SERVER,Term).

uptime(MSU) ->
    calendar:time_difference(calendar:now_to_local_time(MSU),calendar:local_time()).

