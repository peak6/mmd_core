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
-module(mmd_msg).
-include("mmd.hrl").
-export([sendMsg/3]).
-export([reply/3]).
-export([close/3]).
-export([mkReply/2]).
-export([dispatch/2,dispatch/3]).
-export([error/4,error/5,mkError/3]).
-export([getBody/1]).
-export([notSupported/2]).

getBody(#channel_create{body=Body}) -> Body;
getBody(#channel_message{body=Body}) -> Body;
getBody(#channel_close{body=Body}) -> Body.

getID(#channel_create{id=Id}) -> Id;
getID(#channel_close{id=Id}) -> Id;
getID(#channel_message{id=Id}) -> Id.

error(To,InMsg,ErrCode,ErrFmt,ErrArgs) ->
    error(To,InMsg,ErrCode,p6str:mkbin(ErrFmt,ErrArgs)).

error(To,InMsg,ErrCode,ErrBody) ->
    dispatch(To,mkError(InMsg,ErrCode,ErrBody)).

close(To,Id,Body) when is_binary(Id) -> dispatch(To,#channel_close{id=Id,body=Body});
close(To,InMsg,Body) -> close(To,getID(InMsg),Body).

reply(To,RefId,OutBody) ->
    dispatch(To,mkReply(RefId,OutBody)).


mkError(#channel_message{id=Id},Code,Body) -> mkError(Id,Code,Body);
mkError(#channel_create{id=Id},Code,Body) -> mkError(Id,Code,Body);
mkError(Id,Code,Body) -> #channel_close{id=Id,body=?error(Code,Body)}.

mkReply(Id,Body) when is_binary(Id) ->
    #channel_message{id=Id,body=Body};
mkReply(#channel_create{id=Id,type=call},Body) ->
    #channel_close{id=Id,body=Body};
mkReply(#channel_create{id=Id},Body) ->
    #channel_message{id=Id,body=Body};
mkReply(#channel_message{id=Id},Body) ->
    #channel_message{id=Id,body=Body}.

sendMsg(To,Id,Body) ->
    dispatch(To,#channel_message{id=Id,body=Body}).

dispatch(To,Msg) ->
    dispatch(self(),To,Msg).

dispatch(From,To,Msg) ->
    To ! {mmd,From,Msg}.

notSupported(From,Msg=#channel_create{type=sub}) ->
    error(From,Msg,?INVALID_REQUEST,<<"Subscriptions are not supported">>);
notSupported(From,Msg=#channel_create{type=call}) ->
    error(From,Msg,?INVALID_REQUEST,<<"Calls are not supported">>);
notSupported(From,Msg=#channel_message{}) ->
    error(From,Msg,?INVALID_REQUEST,<<"Messages are not supported">>);
notSupported(From,Msg=#channel_close{})  -> ?lwarn("Received unexpected close from: ~p / ~p",[From,Msg]).



%% vim: ts=4:sts=4:sw=4:et:sta:
