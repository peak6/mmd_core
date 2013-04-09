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
-export([handleExit/3, process_local/2, process_local/3, process_local/4]).
-export([process_local_set_data/3, process_local_set_data/4]).
-export([process_remote/3, process_remote/4, process_remote_get_data/3]).
-export([process_down/2]).
-export([sendAll/2, send_all_matching/2]).
-export([refToIds/2, removeRef/2]).
-export([close_all/2]).
-export([chan_count/1]).

-record(state, {tid}).
-define(tid(S), S#state.tid).

-record(chan, {id='_', svc='_', remote='_', data='_', ref='_'}).

new() -> #state{tid = ets:new(channel_mgr, [set, {keypos, 2}])}.
chan_count(State) ->
    ets:info(?tid(State),size).

close_all(State,Body) ->
    ets:foldl(fun(#chan{id=Id,remote=Pid,ref=Ref},S) ->
		      demonitor(Ref),
		      fire(Pid,#channel_close{id=Id,body=Body}),
		      S
	      end, State, ?tid(State)),
    ets:delete_all_objects(?tid(State)).

sendAll(State, Body) ->
    ets:foldl(fun(#chan{id = Id, remote = Ref}, S) ->
		      fire(Ref,#channel_message{id=Id,body=Body}),
		      S
	      end, State, ?tid(State)).

send_all_matching(F, State) ->
    ets:foldl(fun(Ch=#chan{id = Id, remote = Ref, data = Data}, S) ->
		      case F(Data) of
			  {true, Body, Data2} ->
			      fire(Ref, #channel_message{id=Id, body=Body}),
			      ets:insert(?tid(State), Ch#chan{data=Data2});
			  {true, Body} ->
			      fire(Ref, #channel_message{id=Id, body=Body});
			  false ->
			      ok
		      end,
		      S
	      end, State, ?tid(State)).

handleExit(State, Pid, _Reason) ->
    lists:foreach(fun(Id) -> unexpectedClose(Id) end, refToIds(State, Pid)),
    State.

process_down(State,{'DOWN',_Ref,process,Pid,Reason}) ->
    case ets:match(?tid(State),#chan{id='$1',remote=Pid}) of
	[] -> {error,no_channels};
	Channels ->
	    {ok,State,lists:map(
			fun([Id]) ->
				?ldebug("Deleting: ~p",[Id]),
				ets:delete(?tid(State),Id),
				#channel_close{id=Id,body=?error(?UNEXPECTED_REMOTE_CHANNEL_CLOSE,"Remote channel closed, reason: ~p",[Reason])}
			end,
			Channels)}
    end.


%% a local channel_create will spawn a channel process that will
%% dispatch the message for us.
process_local(State, M) -> process_local(State, M, undefined).
process_local(State, M, Cfg) -> process_local(State, M, Cfg, undefined).
process_local(State=#state{tid=Tid}, M=#channel_create{id=Id, service=Svc}, Cfg, Data) ->
    case ets:lookup(Tid,Id) of
	[] ->
	    {ok,Pid} = client_channel:new(self(),M,Cfg),
	    Ref = monitor(process,Pid),
	    NewEntry = #chan{id = Id, remote = Pid, data = Data, ref = Ref, svc=Svc},
	    true = ets:insert_new(Tid, NewEntry),
	    {State,[]};
	Other ->
	    ?lerr("Client attempted to create a duplicate chanel ID\n\tMessage: ~p\n\tCurrent entry: ~p",[M,Other]),
	    exit({error,duplicate_channel_id})
    end;

process_local(State, M=#channel_message{id=Id}, _Cfg, _Data) ->
    case ets:lookup(?tid(State), Id) of
        [] -> {State, noSuchChannel(Id)};
        [#chan{remote=Pid}] ->
	    fire(Pid,M),
	    {State, []}
    end;

process_local(State, M=#channel_close{id=Id}, _Cfg, _Data) ->
    case ets:lookup(?tid(State),Id) of
        [] ->
            ?lwarn("Attempt to close unknown channel: ~p",[Id]),
            {State, noSuchChannel(Id)};
	[#chan{remote=Pid,id=Id,ref=Ref}] ->
	    demonitor(Ref,[flush]),
	    fire(Pid,M),
	    ets:delete(?tid(State),Id),
	    {State,[]}
    end.

process_local_set_data(State, M, Data) ->
    process_local_set_data(State, M, undefined, Data).

process_local_set_data(State, M=#channel_message{id=Id}, _Cfg, Data) ->
    case ets:lookup(?tid(State), Id) of
        [] -> {State, noSuchChannel(Id)};
        [Ch = #chan{remote = Pid}] -> fire(Pid,M),
				      ets:insert(?tid(State), Ch#chan{data = Data}),
				      {State, []}
    end.

process_remote(State, From, M) -> process_remote(State, From, M, undefined).
process_remote(State=#state{tid=Tid}, From, M=#channel_create{id=Id, service=Svc}, Data) ->
    Ref = monitor(process,From),
    Entry = #chan{id = Id, remote = From, data=Data,ref=Ref,svc=Svc},
    case ets:insert_new(Tid,Entry) of
        true -> 
	    {State, M};
        false -> 
	    demonitor(Ref,[flush]),
	    ?lerr("Remote duplicate channel id detected\n\tIncoming message: ~p\n\tCurrent entry: ~p\n\tDuplicate entry: ~p",[M,ets:lookup(Tid,Id),Entry]),
	    mmd_msg:error(From,M,?INVALID_CHANNEL,<<"Server detected a duplicate channel ID">>),
	    {State,[]}
    end;

process_remote(State, From, M=#channel_message{id=Id}, _Data) ->
    case ets:member(?tid(State), Id) of
        false ->
            ?lwarn("Unknown channel: ~p from: ~p",[Id,From]),
            {State,[]};
        true -> {State, M}
    end;
process_remote(State,_From,M=#channel_close{id=Id}, _Data) ->
    case ets:lookup(?tid(State),Id) of
	[] -> {State,M};
	[#chan{ref=Ref}] ->
	    demonitor(Ref,[flush]),
	    ets:delete(?tid(State), Id),
	    {State, M}
    end.

process_remote_get_data(State, From, M=#channel_message{id=Id}) ->
    case ets:lookup(?tid(State), Id) of
        [] ->
            ?lwarn("Unknown channel: ~p from: ~p",[Id,From]),
            State;
        [#chan{data=Data}] -> {State, M, Data}
    end;
process_remote_get_data(State,_From,M=#channel_close{id=Id}) ->
    case ets:lookup(?tid(State),Id) of
	[] -> {State, M};
	[#chan{ref=Ref, data=Data}] ->
	    demonitor(Ref,[flush]),
	    ets:delete(?tid(State), Id),
	    {State, M, Data}
    end.

fire(To,Msg) -> fire(self(),To,Msg).

fire(From,To,Msg) -> 
    To ! {mmd,From,Msg}.

refToIds(State, Ref) ->
    ets:select(?tid(State),
	       [{#chan{id = '$1', remote = Ref, _ = '_'}, [], ['$1']}]).

removeRef(State, Ref) ->
    ets:match_delete(?tid(State), [#chan{remote = Ref, _ = '_'}]),
    State.

%% dupId(From,Id) -> fire(From,dupId(Id)).
%% dupId(Id) -> err(Id,?INVALID_CHANNEL,<<"Duplicate channel id detected.">>).

unexpectedClose(Id) ->
    selfError(Id,?UNEXPECTED_REMOTE_CHANNEL_CLOSE,<<"Connection to the remote channel was lost.">>).

noSuchChannel(Id) ->
    selfError(Id,?INVALID_CHANNEL,<<"Channel not found.">>).

selfError(Id,Code,Msg) ->
    err(Id,Code,Msg).

%%err(Id,Code,Format,Args) -> err(Id,Code,p6str:mkbin(Format,Args)).
err(Id,Code,Body) -> #channel_close{id=Id,body=?error(Code,Body)}.
