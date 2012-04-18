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
%%% Created : Sun Apr 24 08:49:40 CDT 2011
%%%-------------------------------------------------------------------
-module(client_channel).

-behaviour(gen_server2).

%% API
-export([new/2,new/3,new/4]).

-export([getState/1,getInfo/1]).
-export([start_link/3,start_link/4]).
%% gen_server2 callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).
-export([prioritise_info/2]).

-include_lib("p6core/include/p6core.hrl").
-include("mmd.hrl").
-include("mmd_cfg.hrl").

-record(state, {mmdCfg,owner,remote,id,type,svc}).
-record(create,{mmdCfg,owner,msg,remote,createTime=p6time:nowAs(ms)}).
%%%===================================================================
%%% API
%%%===================================================================

new(OwnerPid,ChanCreate,SvcPid,Cfg=#mmd_cfg{}) when is_pid(SvcPid) -> client_channel_sup:new(OwnerPid,ChanCreate,SvcPid,Cfg).
new(OwnerPid,ChanCreate,SvcPid) when is_pid(SvcPid) -> new(OwnerPid,ChanCreate,SvcPid,#mmd_cfg{});
new(OwnerPid,ChanCreate,Cfg=#mmd_cfg{}) -> client_channel_sup:new(OwnerPid,ChanCreate,Cfg).

new(OwnerPid,ChanCreate) -> client_channel_sup:new(OwnerPid,ChanCreate,#mmd_cfg{}).

getState(Pid) when is_pid(Pid) -> call(Pid,getState).
getInfo(Pid) -> call(Pid,getInfo).

start_link(Owner,ChanCreate,SvcPid,Cfg) ->
    gen_server2:start_link(?MODULE,[Owner,ChanCreate,SvcPid,Cfg],[]).
start_link(Owner,ChanCreate,Cfg) ->
    gen_server2:start_link(?MODULE,[Owner,ChanCreate,Cfg],[]).


%%%===================================================================
%%% gen_server2 callbacks
%%%===================================================================
init([Owner,CC,Pid,Cfg]) ->
    process_flag(trap_exit,true),
    link(Owner),
    {ok,#create{mmdCfg=Cfg,owner=Owner,msg=CC,remote=Pid},0};
init([Owner,CC,Cfg]) ->
    process_flag(trap_exit,true),
    link(Owner),
    {ok,#create{mmdCfg=Cfg,owner=Owner,msg=CC},0}.


handle_call(getInfo,_From,Create=#create{}) -> {reply,?DUMP_REC(create,Create),Create};
handle_call(getInfo,_From,State) -> {reply,?DUMP_REC(state,State),State};
handle_call(getState,_From,State) -> {reply,State,State};
handle_call(Request, From, State) ->
    ?lwarn("Unexpected handle_call(~p, ~p, ~p)",[Request,From,State]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    ?lwarn("Unexpected handle_cast(~p, ~p)",[Msg,State]),
    {noreply, State}.

prioritise_info(_Msg,_State) ->
    0.

handle_info({'EXIT',Pid,_},#state{owner=Pid,remote=R,id=Id}) ->
    unlinkRef(R),
    fire(R,#channel_close{id=Id,body=?error(?UNEXPECTED_REMOTE_CHANNEL_CLOSE,
                                            <<"Connection to the remote channel was lost.">>)}),
    {stop,normal,nostate};
handle_info({'EXIT',Pid,_},#state{owner=O,remote=Pid,id=Id}) ->
    unlinkRef(O),
    fire(O,#channel_close{id=Id,body=?error(?UNEXPECTED_REMOTE_CHANNEL_CLOSE,
                                            <<"Connection to the remote channel was lost.">>)}),
    {stop,normal,nostate};


handle_info({mmd,From,CC=#channel_close{}}, State) ->
    fire(resolveDest(From,State),CC),
    unlinkAll(State),
    {stop,normal,nostate};

handle_info({mmd,From,Msg}, State) ->
    fire(resolveDest(From,State),Msg),
    {noreply,State};

handle_info(timeout,C=#create{}) -> initChannel(C);

handle_info(Info,State) ->
    ?lwarn("Unexpected handle_info(~p, ~p)",[Info,State]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

resolveDest(From,#state{owner=From,remote=R}) -> R;
resolveDest(From,#state{owner=O,remote=From}) -> O;
resolveDest(From,#state{owner=O,remote={mod,_}}) when From == self() -> O;
resolveDest(From,State) -> ?lwarn("Can't resolve: ~p -> ~p",[From,State]), error({bad_state,From,State}).


%%%===================================================================
%%% Internal functions
%%%===================================================================
nextTimeout(Create=#create{owner=Owner,createTime=CT,remote=RPid,msg=#channel_create{service=Svc,id=Id,timeout=Timeout}}) ->
    case p6time:timeLeft(ms,CT+Timeout) of
        N when N < 100 ->
            unlink(Owner),
            fire(Owner,#channel_close{id=Id,body=?error(?SERVICE_BUSY,"Service '~p' registered, but busy.",[Svc])}),
            ?lwarn("Timeout waiting for service ~p/~p to be avaliable",[Svc,RPid]),
            {stop,normal,nostate};
        _N ->
%%            ?ldebug("Service busy, will retry: ~p",[Create]),
            {noreply,Create,100}
    end.



initChannel(Create=#create{owner=Owner,msg=CC=#channel_create{id=Id,service=Svc},remote=Pid}) when is_pid(Pid) ->
    case fire(Pid,CC) of
        {ok,Pid} ->
            link(Pid),
            create_tracker:incr(Svc),
            {noreply,#state{owner=Owner,remote=Pid,id=Id,type=client,svc=Svc}};
        Other -> ?lwarn("Failed to setup directed channel to: ~p, reason: ~p",[Pid,Other]),
                 nextTimeout(Create)
    end;

initChannel(Create=#create{mmdCfg=MMDCfg,owner=Owner,msg=CC=#channel_create{service=Svc,id=Id}}) ->
    case services:findBalanced(Svc) of
        [] ->
            fire(Owner,#channel_close{id=Id,body=?error(?SERVICE_NOT_FOUND,"Service '~p' not found.",[Svc])}),
            unlink(Owner),
            ?linfo("~p asked for non-existant service: ~p",[Owner,Svc]),
            {stop,normal,nostate};
        Entries ->
            case fire(Entries,CC) of
                {ok,Pid} ->
                    link(Pid),
                    create_tracker:incr(Svc),
                    case CC#channel_create.type of
                        call -> ok;
                        sub ->
                            case MMDCfg#mmd_cfg.ackSub of
                                false -> ok;
                                true ->
                                    fire(Owner,#channel_message{id=Id,body= <<"$ack$">>})
                            end
                    end,
                    {noreply,#state{owner=Owner,remote=Pid,id=Id,type=client,svc=Svc}};
                {error,retry} -> nextTimeout(Create);
                Other -> ?lwarn("Failed to create channel, reason: ~p, targets: ~p",[Other,Entries]),
                         fire(Owner,#channel_close{id=Id,body=?error(?SERVICE_ERROR,"Service '~p' failure: ~p",[Svc,Other])}),
                         {stop,normal,nostate}
            end
    end.

fire([],_Msg) ->
    {error,all_bad};
fire([[_,Pid,_]|Pids], M)->
    case fire(Pid,M) of
        {ok,Pid} ->
            case catch link(Pid) of
                true -> {ok,Pid};
                {'EXIT',{noproc,_}} ->
                    ?lwarn("Channel created but process died before linking: ~p",[Pid]),
                    {error,bad_service}
            end;
        {error,retry} ->
            fire(Pids,M);
        Other -> ?lwarn("Aborting request, failed to dispatch ~p to: ~p, reason: ~p",[M,Pid,Other]),
		 {error,service_failed}
    end;

fire(undefined,M) -> ?lerr("Unable to send message to 'undefined': ~p",[M]),
                     {error,bad_pid};
fire([_,Pid,_],Msg) -> fire(Pid,Msg);
fire(Pid,Msg) when is_pid(Pid) ->
    case catch gen_server2:call(Pid,{mmd,self(),Msg},?CHANNEL_DISPATCH_TIMEOUT) of
        ok -> {ok,Pid};
        {ok, NewPid} -> {ok, NewPid};
        {'EXIT',{Reason,_}} -> {error,Reason};
        {error,waiting_ack} -> {error,retry};
        Other -> ?linfo("Unexpected return when attempting to dispatch to: ~p, return was: ~p",[Pid,Other]),
                 {error,Other}
    end.

unlinkAll(#state{owner=O,remote=R}) -> unlinkRef(O),
                                       unlinkRef(R).
unlinkRef(Pid) when is_pid(Pid) -> unlink(Pid);
unlinkRef(_Other) -> ok.

call(Pid,Term) -> gen_server2:call(Pid,Term).

%% vim: ts=4:sts=4:sw=4:et:sta:
