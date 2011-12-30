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
-module(pub_service).

-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include_lib("p6core/include/logger.hrl").
-include("mmd.hrl").

-record(state, {chans, sub}).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Name, SortKey) ->
    gen_server:start_link({local, list_to_atom(Name ++ "_pub")},
			  ?MODULE, {Name, SortKey}, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init({Name, SortKey}) ->
    ?linfo("Registering for: ~s.pub", [Name]),
    services:regUnique(list_to_atom(Name ++ ".pub"), SortKey),
    {ok, #state{chans=channel_mgr:new(), sub=list_to_atom(Name ++ "_sub")}}.

handle_call({mmd, From, Msg}, _From, State=#state{chans=Chans, sub=Sub}) ->
    NewChans3 =
	case channel_mgr:processIn(Chans, From, Msg) of
	    {NewChans, CC=#channel_create{type=call, body=Body}} ->
		sub_service:pub(Sub, Body),
		case channel_mgr:processOut(NewChans,
					    mmd_msg:mkReply(CC, null)) of
		    {NewChans2, []} -> NewChans2;
		    {NewChans2, Other} ->
			?linfo("Dunno what to do with: ~p",[Other]),
			NewChans2
		end;
	    {NewChans, #channel_create{type=sub, body = <<?NULL>>}} ->
		NewChans;
	    {NewChans, #channel_create{type=sub, body=Body}} ->
		sub_service:pub(Sub, Body),
		NewChans;
	    {NewChans, #channel_message{body=Body}} ->
		sub_service:pub(Sub, Body),
		NewChans;
	    {NewChans, #channel_close{body = <<?NULL>>}} ->
		NewChans;
	    {NewChans, #channel_close{body=Body}} ->
		sub_service:pub(Sub, Body),
		NewChans
	end,
    {reply, ok, State#state{chans=NewChans3}};

handle_call(Request, From, State) ->
    ?lwarn("Unexpected handle_call(~p, ~p, ~p)",[Request,From,State]),
    {reply, ok, State}.

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
