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
%%% Created : Thr May 03 09:19:16 CDT 2011
%%%-------------------------------------------------------------------
-module(pub).

-export([start_link/0]).
-export([xformEntryAdd/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("p6core/include/logger.hrl").
-include("mmd.hrl").
-include_lib("p6core/include/dmap.hrl").

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, pub}, ?MODULE, [], []).

xformEntryAdd(E) -> E.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    p6dmap:new(subs, ?MODULE),
    services:regLocal(pub),
    {ok, channel_mgr:new()}.

handle_call({mmd, From, Msg=#channel_create{type=call, body=Raw}},
            _From, Chans) ->
    case channel_mgr:process_remote(Chans, From, Msg) of
        {NewChans, CC} ->
            R = case mmd_decode:decode(Raw) of
                    {?map(M), _} ->
                        case p6props:any([<<"topic">>, <<"body">>], M) of
                            [<<Topic/binary>>, Body] when Body =/= undefined ->
                                mmd_msg:mkReply(CC, pub(Topic, Body));
                            _ -> bad_call(CC)
                        end;
                    _ -> bad_call(CC)
                end,
            {Chans2, _} = channel_mgr:process_local(NewChans, R),
            {reply, ok, Chans2}
     end;

handle_call({mmd, From, Msg=#channel_create{type=sub, body=Raw}},
            _From, Chans) ->
    Topic = case mmd_decode:decode(Raw) of
                {<<T/binary>>, _} -> T;
                _ -> undefined
            end,
    case channel_mgr:process_remote(Chans, From, Msg, Topic) of
         {NewChans, CC} ->
            Chans3 =
                case Topic of
                    undefined ->
                        {Chans2, _} =
                            channel_mgr:process_local(
                              NewChans,
                              mmd_msg:mkError(
                                CC,
                                ?INVALID_REQUEST,
                                <<"'pub' must be subscribed with a string "
                                  "of the topic to publish to for each "
                                  "ChannelMessage on this channel">>)),
			Chans2;
                    _ -> NewChans
                end,
            {reply, ok, Chans3}
    end;

handle_call({mmd, From, Msg=#channel_message{body=Body}}, _From, Chans) ->
    case channel_mgr:process_remote_get_data(Chans, From, Msg) of
        {NewChans, _CM, Topic} ->
            pub(Topic, Body),
            {reply, ok, NewChans};
        NewChans ->
            NewChans
    end;

handle_call({mmd, From, Msg=#channel_close{}}, _From, Chans) ->
    {NewChans, _} = channel_mgr:process_remote(Chans, From, Msg),
    {reply, ok, NewChans};

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

bad_call(CC) ->
    mmd_msg:mkError(
      CC,
      ?INVALID_REQUEST,
      <<"'pub' must be called with a map of the form: "
        "{'topic': String, 'body': Any}">>).

pub(Topic, Body) ->
    lists:foldl(
      fun ([ChanPid, {SubPid, ChanId}], N) ->
              ChanPid ! {mmd, SubPid, #channel_message{id=ChanId, body=Body}},
              N + 1
      end,
      0, p6dmap:get(subs, Topic)).
