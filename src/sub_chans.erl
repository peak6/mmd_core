%% Copyright 2012 PEAK6 Investments, L.P.
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
%%% @copyright (C) 2012, PEAK6 Investments, L.P.
%%% @doc
%%%
%%% @end
%%% Created : Mon May 21 09:23:59 CDT 2011
%%%-------------------------------------------------------------------
-module(sub_chans).

-export([start_link/0]).
-export([xformEntryAdd/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("p6core/include/logger.hrl").
-include("mmd.hrl").
-include_lib("p6core/include/dmap.hrl").

start_link() ->
    gen_server:start_link({local, sub_chans}, ?MODULE, [], []).

xformEntryAdd(E) -> E.

init([]) ->
    p6dmap:new(subs, ?MODULE),
    services:regLocal('sub.chans'),
    timer:send_interval(100, update_subs),
    {ok, channel_mgr:new()}.

subs(Topic) ->
    lists:foldl(
      fun ([_ChanPid, {_SubPid, ChanId}], ChanIds) ->
	      [?uuid(ChanId) | ChanIds]
      end,
      [], p6dmap:get(subs, Topic)).

handle_call({mmd, From, Msg}, _From, Chans) ->
    {reply, ok,
     case channel_mgr:process_remote(Chans, From, Msg) of
         {NewChans, CC=#channel_create{type=call, body=Raw}} ->
	     {NewChans2, []} =
		 channel_mgr:process_local(
		   NewChans,
		   case mmd_decode:decode(Raw) of
		       {<<Topic/binary>>, _} ->
			   mmd_msg:mkReply(CC, subs(Topic));
		       _ ->
			   mmd_msg:mkError(
			     CC,
			     ?INVALID_REQUEST,
			     <<"'subs' must be called with a topic to "
			       "list subscribers for">>)
		   end),
	     NewChans2;

         {NewChans, CC=#channel_create{type=sub, body=Raw}} ->
	     {NewChans2, []} =
		 case mmd_decode:decode(Raw) of
		       {<<Topic/binary>>, _} ->
			 Subs = subs(Topic),
			 channel_mgr:process_local_set_data(
			   NewChans,
			   mmd_msg:mkReply(CC, Subs),
			   {Topic, Subs});
		       _ ->
			 channel_mgr:process_local(
			   NewChans,
			   mmd_msg:mkError(
			     CC,
			     ?INVALID_REQUEST,
			     <<"'subs' must be subscribed with a topic to "
			       "list subscribers for">>))
		   end,
	     NewChans2;
         {NewChans, CM=#channel_message{}} ->
             {Chans2, _M} =
                 channel_mgr:process_local(
                   NewChans,
                   mmd_msg:mkError(CM,
                                   ?INVALID_REQUEST,
                                   <<"'sub' doesn't accept channel message">>)),
             Chans2;
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

handle_info(update_subs, Chans) ->
    channel_mgr:send_all_matching(
      fun ({Topic, SubChans}) ->
	      SubChans2 = subs(Topic),
	      case {SubChans -- SubChans2, SubChans2 -- SubChans} of
		  {[], []} -> false;
		  {[], A} ->
		      {true,
		       ?map([{topic, Topic}, {added, ?array(A)}]),
		       {Topic, SubChans2}};
		  {R, []} ->
		      {true,
		       ?map([{topic, Topic}, {removed, ?array(R)}]),
		       {Topic, SubChans2}};
		  {R, A} ->
		      {true,
		       ?map([{topic, Topic},
			     {added, ?array(A)}, {removed, ?array(R)}]),
		       {Topic, SubChans2}}
	      end
      end,
      Chans),
    {noreply, Chans};
handle_info(Info, State) ->
    ?lwarn("Unexpected handle_info(~p, ~p)",[Info, State]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
