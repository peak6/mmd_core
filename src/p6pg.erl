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

%% @doc Watches / Manages an abitrary group of processes.
-module(p6pg).
-behaviour(gen_server).
-include("logger.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/1,start_link/2]).
-export([add/2,add/3,del/2]).
-export([clear/2,set/3,get/2,cas/4]).
-export([call/2,cast/2,send/2]).
-export([add_watcher/2,del_watcher/2]).
-export([members/1,refs/1,memberData/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% gen_server State
-record(state,{name,watchers,members=[]}).
-record(ref,{pid,ref,count,data}).

-define(newRef(Pid), #ref{pid=Pid,ref=monitor(process,Pid),count=1}).
-define(newRef(Pid,Data), #ref{pid=Pid,ref=monitor(process,Pid),data=Data,count=1}).
-define(takePid(Pid,Members), lists:keytake(Pid,#ref.pid,Members)).
-define(findPid(Pid,Members), lists:keyfind(Pid,#ref.pid,Members)).


start_link(Name) ->
    start_link(Name,[]).
start_link(Name,Options) ->
    gen_server:start_link({local,Name}, ?MODULE, [Name,Options], []).

add_watcher(Group,Pid) -> p6pg:add(watchers(Group),Pid).
del_watcher(Group,Pid) -> p6pg:del(watchers(Group),Pid).

add(Group,Pid,Data) -> docall(Group,{add,Pid,Data}).
add(Group,Pid) -> docall(Group,{add,Pid}).
get(Group,Pid) -> docall(Group,{get,Pid}).
set(Group,Pid,Val) -> docall(Group,{set,Pid,Val}).
cas(Group,Pid,Expected,New) -> docall(Group,{cas,Pid,Expected,New}).
clear(Group,Pid) -> set(Group,Pid,undefined).

del(Group,Pid) -> docall(Group,{del,Pid}).
memberData(Group) -> [ {P,D} || #ref{pid=P,data=D} <- refs(Group) ].
members(Group) -> docall(Group,members).
refs(Group) -> docall(Group,refs).
watchers(Group) -> docall(Group,watchers).

cast(Group,Term) -> lists:foreach(fun(P) -> docast(P,Term) end, members(Group)).
call(Group,Term) -> [ docall(P,Term) || P <- members(Group) ].
send(Group,Term) -> lists:foreach(fun(P) -> P ! Term end, members(Group)).



%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init([Name,Options]) when is_list(Options) ->
    case proplists:get_value(watcher,Options) of
        undefined -> WG = undefined;
        Watcher ->
            WG = p6str:mkatom("~p_watcher",[Name]),
            {ok,_} = p6pg:start_link(WG),
            p6pg:add(WG,Watcher,unique)
    end,
    {ok, #state{name=Name,watchers=WG}};
init([Name,Option]) -> init([Name,[Option]]).


handle_call(refs,_From,State=#state{members=M}) -> {reply,M,State};
handle_call(members,_From,State) -> {reply,memberList(State),State};
handle_call(watchers,_From,State=#state{watchers=undefined,name=N}) ->
    W = p6str:mkatom("~p_watchers",[N]),
    {ok,_} = p6pg:start_link(W),
    {reply,W,State#state{watchers=W}};

handle_call({get,Pid}, _From,State=#state{members=Members}) ->
    case ?findPid(Pid,Members) of
        false -> {reply,not_found,State};
        #ref{data=V} -> {reply,V,State}
    end;

handle_call({cas,Pid,Exp,New},_From,State=#state{members=Members}) ->
    case ?takePid(Pid,Members) of
        false -> {reply,not_found,State};
        {value,R=#ref{data=Exp},NewMem} ->
            {reply,ok,State#state{members=[R#ref{data=New}|NewMem]}};
        {value,#ref{data=X},_} -> {reply,{error,X},State}
    end;
handle_call({set,Pid,Val}, _From, State=#state{members=Members}) ->
    case ?takePid(Pid,Members) of
        false ->
            {reply,not_found,State};
        {value,R,NewMem} ->
            {reply,ok,State#state{members=[R#ref{data=Val}|NewMem]}}
    end;


handle_call({add,Pid},_From, State=#state{members=Members}) ->
    case ?takePid(Pid,Members) of
        false ->
            sendWatchers(State,groupMemberUp,Pid),
            {reply,ok,State#state{members=[?newRef(Pid)|Members]}};
        {value,R,NewMem} ->
            {reply,ok,State#state{members=[inc(R)|NewMem]}}
    end;

handle_call({add,Pid,Data},_From, State=#state{members=Members}) ->
    case ?takePid(Pid,Members) of
        false ->
            sendWatchers(State,groupMemberUp,Pid),
            {reply,ok,State#state{members=[?newRef(Pid,Data)|Members]}};
        {value,R,_} -> {reply,{exists,R#ref.data},State}
    end;
handle_call({del,Pid},_From, State=#state{members=Members}) ->
    case ?takePid(Pid,Members) of
        false ->
            {reply,not_found,State};
        {value,#ref{ref=R,count=1},NewMem} ->
            demonitor(R,[flush]),
            sendWatchers(State,groupMemberAdded,Pid),
            {reply,ok,State#state{members=NewMem}};
        {value,R,NewMem} ->
            {reply,ok,State#state{members=[dec(R)|NewMem]}}
    end;

handle_call(Request, From, State) ->
    ?linfo("Unexpected call: ~p from: ~p, with state: ~p",[Request,From,State]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    ?linfo("Unexpected cast: ~p, with state: ~p",[Msg,State]),
    {noreply, State}.

handle_info({'DOWN',_Ref,process,Pid,Reason},State=#state{members=Members,name=Name,watchers=W}) ->
    case ?takePid(Pid,Members) of
        {value,R,NewMem} ->
            sendWatchers(W,{groupMemberDown,Name,R#ref.pid}),
            {noreply,State#state{members=NewMem}};
        false -> ?lwarn("Unknown process: ~p died: ~p",[Pid,Reason]),
                 {noreply,State}
    end;

handle_info(Info, State) ->
    ?linfo("Unexpected info: ~p, with state: ~p",[Info,State]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
sendWatchers(undefined,_Term) -> ok;
sendWatchers(W,Term) -> p6pg:send(W,Term).
sendWatchers(#state{watchers=undefined},_,_) -> ok;
sendWatchers(#state{watchers=W,name=N},Atom,Pid) ->
    sendWatchers(W,{Atom,N,Pid}).

docast(Pid, Term) -> gen_server:cast(Pid, Term).
docall(Pid, Term) -> gen_server:call(Pid, Term).
inc(Ref=#ref{count=C}) -> Ref#ref{count=C+1}.
dec(Ref=#ref{count=C}) when C > 1 -> Ref#ref{count=C-1}.
memberList(#state{members=M}) -> [P || #ref{pid=P} <- M].
