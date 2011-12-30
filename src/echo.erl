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

-export([handleCall/2,handleSubscribe/2,handleMessage/2,handleClose/2]).
-behaviour(mmd_service).

handleSubscribe(From,ChanCreate=#channel_create{id=Id,body=Body}) ->
    ?linfo("Echo (subscribe): ~p / ~p",[Id,Body]),
    mmd_msg:reply(From,ChanCreate,Body),
    ok.

handleCall(_From,_ChanCreate=#channel_create{body= <<"fail">>}) ->
    exit({error,asked_to_fail});
handleCall(From,ChanCreate=#channel_create{body=Body}) ->
%%    ?linfo("Echo (call) --\n~p",[?DUMP_REC(channel_create,ChanCreate)]),
    case application:get_env(esleep) of
        undefined -> ok;
        {ok,N} -> timer:sleep(N),
             ?linfo("Replying after: ~p",[N])
    end,
    mmd_msg:reply(From,ChanCreate,Body),
    ok.

handleMessage(From,ChanMsg=#channel_message{id=Id,body=Body}) ->
    ?linfo("Echo (msg): ~p / ~p",[Id,Body]),
    mmd_msg:reply(From,ChanMsg,Body),
    ok.

handleClose(_From,#channel_close{id=Id,body=Body}) ->
    ?linfo("Echo (close): ~p / ~p",[Id,Body]),
    ok.

%% vim: ts=4:sts=4:sw=4:et:sta:
