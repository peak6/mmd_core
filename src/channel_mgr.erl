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
-export([refToIds/2, removeRef/2]).

-record(state, {tid}).
-define(tid(S), S#state.tid).

new() -> #state{tid = ets:new(channel_mgr, [set])}.

sendAll(State, Body) ->
    ets:foldl(fun({Id, Ref}, S) ->
		      fire(Ref,#channel_message{id=Id,body=Body}),
		      S
	      end, State, ?tid(State)).

handleExit(State, Pid, _Reason) ->
    lists:foreach(fun(Id) -> unexpectedClose(Id) end, refToIds(State, Pid)),
    State.

%% a local create_channel will spawn a channel process that will
%% dispatch the message for us.
processOut(State, M) -> processOut(State, M, undefined).
processOut(State, M=#channel_create{id=Id}, Cfg) ->
    case ets:member(?tid(State), Id) of
        true -> {State, dupId(Id)};
        false ->
            case ets:info(?tid(State), size) of
                N when N > ?MAX_CONCURRENT_CHANNELS -> {State, maxChans(Id,N)};
                _ ->
                    {ok,Pid} = client_channel:new(self(),M,Cfg),
		    ets:insert(?tid(State), {Id, Pid}),
                    {State, []}
            end
    end;

processOut(State, M=#channel_message{id=Id},_Cfg) ->
    case ets:lookup_element(?tid(State), Id, 2) of
        badarg -> {State, noSuchChannel(Id)};
        Pid -> fire(Pid,M),
	       {State, []}
    end;

processOut(State,M=#channel_close{id=Id},_Cfg) ->
    case ets:lookup_element(?tid(State), Id, 2) of
        badarg ->
            ?lwarn("Attempt to close unknown channel: ~p",[Id]),
            {State, noSuchChannel(Id)};
        Pid -> fire(Pid, M),
	       ets:delete(?tid(State), Id),
               {State, []}
    end.

processIn(State, From,M=#channel_create{id=Id}) ->
    case ets:member(?tid(State), Id) of
        false -> ets:insert(?tid(State), {Id, From}),
		 {State, M};
        true -> dupId(From,Id),
                State
    end;

processIn(State, From, M=#channel_message{id=Id}) ->
    case ets:member(?tid(State), Id) of
        false ->
            ?lwarn("Unknown channel: ~p from: ~p",[Id,From]),
            State;
        true -> {State, M}
    end;
processIn(State,_From,M=#channel_close{id=Id}) ->
    ets:delete(?tid(State), Id),
    {State, M}.

fire(To,Msg) -> fire(self(),To,Msg).
fire(From,To,Msg) -> To ! {mmd,From,Msg}.

refToIds(State, Ref) ->
    ets:select(?tid(State), [{{'$1', Ref}, [], ['$1']}]).

removeRef(State, Ref) ->
    ets:match_delete(?tid(State), [{'_', Ref}]),
    State.

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
