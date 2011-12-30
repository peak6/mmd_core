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
%%% @author sparish@peak6.com
%%% @copyright (C) 2011, PEAK6 Investments, L.P.
%%% @doc
%%%
%%% @end
%%% Created : Tue Jun 29 10:16:30 CDT 2011
%%%-------------------------------------------------------------------
-module(sub_service).

-export([start_link/2, pub/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include_lib("p6core/include/logger.hrl").
-include("mmd.hrl").

%%%===================================================================
%%% API
%%%===================================================================
start_link(Name, SortKey) ->
    gen_server:start_link({local, list_to_atom(Name ++ "_sub")},
			  ?MODULE, {Name, SortKey}, []).

pub(SubName, Msg) ->
    gen_server:cast(SubName, {pub, Msg}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init({Name, SortKey}) ->
    ?linfo("Registering for: ~s.sub", [Name]),
    services:regUnique(list_to_atom(Name ++ ".sub"), SortKey),
    {ok, channel_mgr:new()}.

handle_call({mmd, From, Msg}, _From, Chans) ->
    {reply, ok,
     case channel_mgr:processIn(Chans, From, Msg) of
	 {NewChans, CC=#channel_create{type=call}} ->
	     R = channel_mgr:processOut(
		   NewChans,
		   mmd_msg:mkError(CC, ?INVALID_REQUEST,
				   <<"Sub doesn't support Call channels">>)),
	     case R of
		 {NewChans2, []} -> NewChans2;
		 {NewChans2, Other} ->
		     ?linfo("Dunno what to do with: ~p",[Other]),
		     NewChans2
	     end;
	 {NewChans, #channel_create{type=sub}} ->
	     NewChans;
	 {NewChans, CM=#channel_message{}} ->
	     channel_mgr:processOut(
	       NewChans,
	       mmd_msg:mkError(CM, ?INVALID_REQUEST,
			       <<"Sub doesn't accept channel message">>));
	 {NewChans, #channel_close{}} ->
	     NewChans
     end};
handle_call(Request, From, State) ->
    ?lwarn("Unexpected handle_call(~p, ~p, ~p)",[Request,From,State]),
    {reply, ok, State}.

handle_cast({pub, Msg}, Chans) ->
    channel_mgr:sendAll(Chans, Msg),
    {noreply, Chans};
handle_cast(Msg, State) ->
    ?lwarn("Unexpected handle_cast(~p, ~p)",[Msg,State]),
    {noreply, State}.

handle_info(Info, State) ->
    ?lwarn("Unexpected handle_info(~p, ~p)",[Info, State]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
