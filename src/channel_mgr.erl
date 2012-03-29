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
-module(channel_mgr).
-include("mmd.hrl").
-include_lib("p6core/include/logger.hrl").
-export([new/0]).
-export([handleExit/3,processOut/2,processOut/3,processIn/3]).
-export([sendAll/2]).

new() -> channels:new().

sendAll(Chans,Body) ->
    channels:each(fun(Id,Ref) -> fire(Ref,#channel_message{id=Id,body=Body}) end, Chans),
    Chans.

handleExit(Chans,Pid,_Reason) -> 
    lists:foreach(fun(Id) -> unexpectedClose(Id) end, channels:refToId(Pid,Chans)),
    Chans.

%% a local create_channel will spawn a channel process that will dispatch the message for us.
processOut(Chans,M) -> processOut(Chans,M,undefined).
processOut(Chans,M=#channel_create{id=Id},Cfg) ->
    case channels:hasId(Id,Chans) of
        true -> {Chans,dupId(Id)};
        false ->
            case length(Chans) of
                N when N > ?MAX_CONCURRENT_CHANNELS -> {Chans,maxChans(Id,N)};
                _ -> 
                    {ok,Pid} = client_channel:new(self(),M,Cfg),
                    {channels:add(Id,Pid,Chans),[]}
            end
    end;

processOut(Chans,M=#channel_message{id=Id},_Cfg) ->
    case channels:idToRef(Id,Chans) of
        not_found -> {Chans,noSuchChannel(Id)};
        Pid -> fire(Pid,M),
               {Chans,[]}
    end;

processOut(Chans,M=#channel_close{id=Id},_Cfg) ->
    case channels:idToRef(Id,Chans) of
        not_found -> 
            ?lwarn("Attempt to close unknown channel: ~p",[Id]),
            {Chans,noSuchChannel(Id)};
        Pid -> fire(Pid,M),
               {channels:removeId(Id,Chans),[]}
    end.


processIn(Chans,From,M=#channel_create{id=Id}) ->
    case channels:hasId(Id,Chans) of
        false -> {channels:add(Id,From,Chans),M};
        true -> dupId(From,Id),
                Chans
    end;
                
processIn(Chans,From,M=#channel_message{id=Id}) ->
    case channels:hasId(Id,Chans) of
        false -> 
            ?lwarn("Unknown channel: ~p from: ~p",[Id,From]),
            Chans;
        true -> {Chans,M}
    end;
processIn(Chans,_From,M=#channel_close{id=Id}) ->
    {channels:removeId(Id,Chans),M}.

fire(To,Msg) -> fire(self(),To,Msg).
fire(From,To,Msg) -> To ! {mmd,From,Msg}. 
                             

maxChans(Id,_N) -> err(Id,?INVALID_CHANNEL,"Maximum channels per connection (~p) reached",[?MAX_CONCURRENT_CHANNELS]).

dupId(From,Id) -> fire(From,dupId(Id)).
dupId(Id) -> err(Id,?INVALID_CHANNEL,<<"Duplicate channel id detected.">>).

unexpectedClose(Id) -> 
    selfError(Id,?UNEXPECTED_REMOTE_CHANNEL_CLOSE,<<"Connection to the remote channel was lost.">>).

noSuchChannel(Id) ->
    selfError(Id,?INVALID_CHANNEL,<<"Channel not found.">>).

selfError(Id,Code,Msg) ->
    err(Id,Code,Msg).

err(Id,Code,Format,Args) -> err(Id,Code,p6str:mkbin(Format,Args)).
err(Id,Code,Body) -> #channel_close{id=Id,body=?error(Code,Body)}.
