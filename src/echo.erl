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
-module(echo).

-include_lib("p6core/include/p6core.hrl").
-include("mmd.hrl").

-export([service_call/2,
	 service_subscribe/2,
	 service_message/2,
	 service_close/2
%%	 ,handle_other/2
	]).

-behaviour(mmd_service).

service_subscribe(Client,CC=#channel_create{body=Body}) ->
    ?ldebug("service_subscribe(~p,~p)",[Client,CC]),
    reply(Body).

service_call(Client,CC=#channel_create{body=Body}) ->
    ?ldebug("service_call(~p,~p)",[Client,CC]),
    reply(Body).

service_message(CM=#channel_message{body=Body},_State) ->
    ?ldebug("service_message(~p)",[CM]),
    reply(Body).

service_close(CC,State) ->
    ?ldebug("service_close(~p,~p)",[CC,State]).
    
reply(<<"fail">>) -> exit({error,asked_to_fail});
reply(Body) -> {reply,Body}.

%% vim: ts=4:sts=4:sw=4:et:sta:
