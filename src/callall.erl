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
%%% Created : Thu Mar 31 07:08:45 CDT 2011
%%%-------------------------------------------------------------------
-module(callall).

-include_lib("p6core/include/logger.hrl").
-include("mmd.hrl").

-export([handleCall/2,handleSubscribe/2,handleMessage/2,handleClose/2]).

-spec(handleCall(From::pid(),Create::#channel_create{}) -> ok).
handleCall(From,CC=#channel_create{timeout=Timeout,auth_token=AT,body=?map(Map)}) ->
    case p6props:any([<<"service">>,<<"body">>],Map) of
        [undefined,_] -> badCall(From,CC);
        [Svc,Body] ->
            case cluster_mmd_call:call(Svc,Body,AT,Timeout) of
                {error,not_found} -> mmd_msg:reply(From,CC,?error(?SERVICE_NOT_FOUND,p6str:mkbin("Service: ~s not found",[Svc])));
                Result when is_list(Result) ->
                    Ret = lists:map(fun(Pair={K,_}) when is_atom(K) -> Pair;
                                       ({K,V}) -> {node(K),V}
                                    end, Result),
                    mmd_msg:reply(From,CC,?map(Ret));
                Other ->
                    ?lerr("Received ~p from cluster_mmd_call:call(~p, ~p, ~p, ~p)",[Other,Svc,Body,AT,Timeout]),
                    mmd_msg:reply(From,CC,?error(?SERVICE_ERROR,p6str:mkbin("Unexpected return from cluster call: ~p",[Other])))
                end
    end;

handleCall(From,CC=#channel_create{}) ->
    badCall(From,CC).

badCall(From,Create) ->
    mmd_msg:error(From,Create,?INVALID_REQUEST,<<"Call takes a map(service,body)">>).

-spec(handleSubscribe(From::pid(),Create::#channel_create{}) -> ok).
handleSubscribe(From,ChanCreate=#channel_create{}) ->
    mmd_msg:error(From,ChanCreate,?INVALID_REQUEST,<<"Subscribe not supported">>).

-spec(handleMessage(From::pid(),Create::#channel_message{}) -> ok).
handleMessage(_From,_ChanMsg=#channel_message{}) ->
    ok.

-spec(handleClose(From::pid(),Create::#channel_close{}) -> ok).
handleClose(_From,_ChanClose=#channel_close{}) ->
    ok.

