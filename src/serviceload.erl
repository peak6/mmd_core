% Copyright 2011 PEAK6 Investments, L.P.
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
-module(serviceload).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([transformHostLoad/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include_lib("p6core/include/logger.hrl").
-include("mmd.hrl").

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

transformHostLoad([_O,_K,{S,SL,_}],Load) -> {S,SL,Load};
transformHostLoad([_O,_K,{SL,_}],Load) -> {SL,Load};
transformHostLoad(_OtherData,_Load) -> '$ignore'.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    services:regLocal('service.load'),
    timer:send_interval(5000,updateHostLoad),
    {ok, #state{}}.

handle_call({mmd,From,CC=#channel_create{type=call,body=?raw(_)}},_From, State) ->
    handle_call({mmd,From,mmd_decode:decodeFull(CC)},_From, State);

handle_call({mmd,From,CC=#channel_create{type=call,body=?map(Map),originator=O}},_From, State) ->
    Count = lists:foldl(fun({N,L},C) ->
                                services:updateSvcLoad(O,N,L), C+1
                        end, 0, Map),
    mmd_msg:reply(From,CC,Count),
    {reply,ok,State};

handle_call(Request, From, State) ->
    ?lwarn("Unexpected handle_call(~p, ~p, ~p)",[Request,From,State]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    ?lwarn("Unexpected handle_cast(~p, ~p)",[Msg,State]),
    {noreply, State}.

handle_info(updateHostLoad,State)->
    NewHL = cpu_load:util(),
    services:transformValues({?MODULE,transformHostLoad,[NewHL]}),
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
