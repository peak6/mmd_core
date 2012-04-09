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
-module(echo2).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([setLoad/1,getState/0,run/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("p6core/include/logger.hrl").
-include("mmd.hrl").

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================
getState() -> gen_server:call(?SERVER,getState).

run(Fun) -> gen_server:call(?SERVER,{run,Fun}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
setLoad(N) -> gen_server:call(?SERVER,{setLoad,N}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    process_flag(trap_exit,true),
    services:regLocal(?SERVER),
    {ok, channel_mgr:new()}.

handle_call({run,Fun},_From,State) -> {reply,Fun(),State};

handle_call(getState,_From,State) ->
    {reply,State,State};

handle_call({setLoad,N},_From,State) ->
    services:updateSvcLoad(?SERVER,N),
    {reply,ok,State};

handle_call({mmd,From,Msg},_From,Chans) ->
    case channel_mgr:processIn(Chans,From,Msg) of
        {NewChans,Msg} -> {reply,ok,process(Msg,NewChans)}
%%        NewChans -> {reply,ok,NewChans}
    end;

handle_call(Request, From, State) ->
    ?lwarn("Unexpected handle_call(~p, ~p, ~p)",[Request,From,State]),
    {reply,ok,State}.

handle_cast(Msg, State) ->
    ?lwarn("Unexpected handle_cast(~p, ~p)",[Msg,State]),
    {noreply, State}.

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
process(#channel_close{},Chans) ->
    Chans;
process(Msg,Chans) ->
%%    ?ldebug("Responding to: ~p",[Msg]),
    case channel_mgr:processOut(Chans,mmd_msg:mkReply(Msg,mmd_msg:getBody(Msg))) of
        {NewChans,[]} -> NewChans;
        {NewChans,Other} ->
            ?linfo("Dunno what to do with: ~p",[Other]),
            NewChans
    end.

%% vim: ts=4:sts=4:sw=4:et:sta:
