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
-export([new/0, new/1]).
-export([handleExit/3,processOut/2,processOut/3,processIn/3]).
-export([sendAll/2]).
-export([refToIds/2, removeRef/2]).

-record(state, {tid, max_chans}).
-define(tid(S), S#state.tid).
-define(max_chans(S), S#state.max_chans).

new() -> new(?MAX_CONCURRENT_CHANNELS).
new(MaxChans) -> #state{tid = ets:new(channel_mgr, [set]),
			max_chans = MaxChans}.

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
    case ets:info(?tid(State), size) > ?max_chans(State) of
	true -> {State, maxChans(Id, ?max_chans(State))};
	false ->
	    {ok,Pid} = client_channel:new(self(),M,Cfg),
	    case ets:insert_new(?tid(State), {Id, Pid}) of
		true -> {State, []};
		false -> {State, dupId(Id)}
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
    case ets:insert_new(?tid(State), {Id, From}) of
        true -> {State, M};
        false -> dupId(From,Id),
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

maxChans(Id, MaxChans) ->
    err(Id, ?INVALID_CHANNEL,
	"Maximum channels per connection (~p) reached", [MaxChans]).

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
