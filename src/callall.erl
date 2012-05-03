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

-export([service_call/2, service_subscribe/2]).

service_call(_C, #channel_create{timeout=TO, auth_token=AT, body=?map(M)}) ->
    case p6props:any([<<"service">>, <<"body">>], M) of
        [undefined, _] -> bad_call();
        [Svc, Body] ->
            case cluster_mmd_call:call(Svc, Body, AT, TO) of
                {error, not_found} ->
		    {error,
		     ?SERVICE_NOT_FOUND,
		     "Service: ~s not found", [Svc]};
                Result when is_list(Result) ->
                    Ret = lists:map(fun(Pair={K,_}) when is_atom(K) -> Pair;
                                       ({K,V}) -> {node(K),V}
                                    end, Result),
                    {reply, ?map(Ret)};
                Other ->
                    ?lerr("Received ~p from "
			  "cluster_mmd_call:call(~p, ~p, ~p, ~p)",
			  [Other, Svc, Body, AT, TO]),
		    {error,
		     ?SERVICE_ERROR,
		     "Unexpected return from cluster call: ~p", [Other]}
                end
    end;

service_call(_Client, #channel_create{}) ->
    bad_call().

bad_call() ->
    {error, ?INVALID_REQUEST, <<"Call takes a map(service, body)">>}.

service_subscribe(_Client, _CC) ->
    {error, ?INVALID_REQUEST, <<"Subscribe not supported">>}.
