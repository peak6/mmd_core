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
%%% Created : Tue Apr 19 15:27:30 CDT 2011
%%%-------------------------------------------------------------------
-module(time_service).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([clearChannels/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include_lib("p6core/include/logger.hrl").
-include("mmd.hrl").

-define(SERVER, ?MODULE). 


%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

clearChannels() ->
    gen_server:call(?SERVER,clear_channels).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    services:regLocal(time),
    timer:send_interval(1000,tick),
    {ok, channel_mgr:new()}.

handle_call({mmd,From,Msg}, _From, Chans) ->
    case channel_mgr:processIn(Chans,From,Msg) of
        {NewChans,CC=#channel_create{}} -> 
            {ReallyNew,[]} = channel_mgr:processOut(NewChans,mmd_msg:mkReply(CC,mkTime())),
            {reply,ok,ReallyNew};
        {NewChans,_} -> {reply,ok,NewChans};
        NewChans -> {reply,ok,NewChans}
    end;
handle_call(clear_channels,_From,Chans) ->
    ?linfo("Dropping channels: ~p",[Chans]),
    {reply,ok,channel_mgr:new()};
handle_call(Request, From, State) ->
    ?lwarn("Unexpected handle_call(~p, ~p, ~p)",[Request,From,State]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    ?lwarn("Unexpected handle_cast(~p, ~p)",[Msg,State]),
    {noreply, State}.

mkTime() ->
    TimeStr = p6str:timeToStr(os:timestamp()),
    TimeUS = ?time(p6time:nowAs(us)),
    ?map([{ts,TimeUS},{time,TimeStr}]).

handle_info(tick,Chans) ->
    channel_mgr:sendAll(Chans,mkTime()),
    {noreply,Chans};
                             
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
