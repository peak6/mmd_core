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
%%% @author Scott R Parish <sparish@peak6.com>
%%% @copyright (C) 2011, PEAK6 Investments, L.P.
%%% @doc
%%%
%%% @end
%%% Created : Tue Jul 06 10:49:30 CDT 2011
%%%-------------------------------------------------------------------
-module(mmd_tcp_client).

-export([start_link/3]).
-export([send/2, call/2, call/3, register/2, unregister/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include_lib("p6core/include/logger.hrl").
-include("mmd.hrl").

-record(state, {sock, svcs, chans, name}).
-record(svc_info, {pid, mon_ref}).
-record(chan_info, {pid, mon_ref}).

start_link(Name, Host, Port) ->
    gen_server:start_link({local, Name}, ?MODULE, {Name, Host, Port}, []).

init({Name, Host, Port}) ->
    ?linfo("mmd_tcp_client: ~p connecting to remote mmd ~p:~p",
	   [Name, Host, Port]),
    {ok, Sock} = gen_tcp:connect(Host, Port,
				 [{packet,4}, binary, {keepalive, true}]),
    ok = gen_tcp:send(Sock, ?latest_vsn_bin),
    {ok, #state{sock=Sock, chans=dict:new(), svcs=dict:new(), name=Name}}.

send(Pid, M) ->
    gen_server:call(Pid, {send, M}).

call(Pid, M) ->
    gen_server:call(Pid, {call, M}).

call(Pid, Svc, Body) ->
    call(Pid,
	 #channel_create{id=p6uuid:next(), service=Svc, type=$C, body=Body}).

reg_action(Pid, Action, SvcName) ->
    M = #channel_create{id=p6uuid:next(),
			service = <<"serviceregistry">>,
			type=$C,
			body={'$map', [{Action, SvcName}]}},
    gen_server:call(Pid, {reg, Action, SvcName, M}).

register(Pid, SvcName) ->
    reg_action(Pid, register, p6str:to_lower_bin(SvcName)).

unregister(Pid, SvcName) ->
    reg_action(Pid, unregister, p6str:to_lower_bin(SvcName)).

chan_add(State=#state{chans=Chans}, Id, Pid) ->
    MR = case Pid of
	     P when is_pid(P) -> erlang:monitor(process, Pid);
	     {P, _Tag} -> erlang:monitor(process, P)
	 end,
    State#state{chans=dict:store(Id, #chan_info{pid=Pid, mon_ref=MR}, Chans)}.

chan_rm(State=#state{chans=Chans}, Id) ->
    case dict:find(Id, Chans) of
	{ok, #chan_info{mon_ref=MR}} ->
	    erlang:demonitor(MR),
	    State#state{chans=dict:erase(Id, Chans)};
	error ->
	    State
    end.

chan_pid(#state{chans=Chans}, Id) ->
    case dict:find(Id, Chans) of
	{ok, #chan_info{pid=Pid}} ->
	    {ok, Pid};
	error -> error
    end.

pid_chans(#state{chans=Chans}, Pid) ->
    dict:fold(fun (Id, #chan_info{pid=P}, Ids) ->
		      if Pid == P -> [Id | Ids];
			 true -> Ids
		      end
	      end, [], Chans).

svc_add(State=#state{svcs=Svcs}, Name, Pid) ->
    MR = case Pid of
	     P when is_pid(P) -> erlang:monitor(process, Pid);
	     {P, _Tag} -> erlang:monitor(process, P)
	 end,
    State#state{svcs=dict:store(Name, #svc_info{pid=Pid, mon_ref=MR}, Svcs)}.

svc_rm(State=#state{svcs=Svcs}, Name) ->
    case dict:find(Name, Svcs) of
	{ok, #svc_info{mon_ref=MR}} ->
	    erlang:demonitor(MR),
	    State#state{svcs=dict:erase(Name, Svcs)};
	error ->
	    State
    end.

svc_pid(#state{svcs=Svcs}, Name) ->
    case dict:find(Name, Svcs) of
	{ok, #svc_info{pid=Pid}} ->
	    {ok, Pid};
	error -> error
    end.

pid_svcs(#state{svcs=Svcs}, Pid) ->
    dict:fold(fun (Name, #svc_info{pid=P}, Names) ->
		      if Pid == P -> [Name | Names];
			 true -> Names
		      end
	      end, [], Svcs).

handle_call({call, M=#channel_create{id=Id}}, From, State=#state{sock=Sock}) ->
    ok = gen_tcp:send(Sock, mmd_encode:encode_message(M)),
    {noreply, chan_add(State, Id, From)};
handle_call({send, M=#channel_create{id=Id}}, {Pid, _Tag},
	    State=#state{sock=Sock}) ->
    ok = gen_tcp:send(Sock, mmd_encode:encode_message(M)),
    {reply, ok, State#state{chans=chan_add(State, Id, Pid)}};
handle_call({send, M=#channel_message{}}, _From, State=#state{sock=Sock}) ->
    gen_tcp:send(Sock, mmd_encode:encode_message(M)),
    {reply, ok, State};
handle_call({send, M=#channel_close{id=Id}}, _From, State=#state{sock=Sock}) ->
    gen_tcp:send(Sock, mmd_encode:encode_message(M)),
    {reply, ok, chan_rm(State, Id)};
handle_call({reg, Action, SvcName, M=#channel_create{id=Id}}, From={Pid, _Tag},
	    State=#state{sock=Sock}) ->
    ok = gen_tcp:send(Sock, mmd_encode:encode_message(M)),
    State2 = case Action of
		 register -> svc_add(State, SvcName, Pid);
		 unregister -> svc_rm(State, SvcName)
	     end,
    {noreply, chan_add(State2, Id, From)};
handle_call(Request, From, State) ->
    ?lwarn("Unexpected handle_call(~p, ~p, ~p)",[Request,From,State]),
    {reply, unknown, State}.

handle_cast(Msg, State) ->
    ?lwarn("Unexpected handle_cast(~p, ~p)",[Msg,State]),
    {noreply, State}.


handle_info({tcp, Sock, Data}, State=#state{name=Name}) ->
    case mmd_decode:decode_head(?raw(Data)) of
	M=#channel_create{service=Svc, id=Id} ->
	    ?linfo("channel_create: service: ~p", [Svc]),
	    case svc_pid(State, Svc) of
		{ok, Pid} ->
		    erlang:send(Pid, {mmd_tcp_client, Name, M}),
		    {noreply, chan_add(State, Id, Pid)};
		error ->
		    Err = mmd_msg:mkError(M, ?SERVICE_NOT_FOUND,
					  <<"Service not found">>),
		    ok = gen_tcp:send(Sock, mmd_encode:encode_message(Err)),
		    {noreply, State}
	    end;
	M=#channel_message{id=Id} ->
	    case chan_pid(State, Id) of
		{ok, Pid} ->
		    erlang:send(Pid, {mmd_tcp_client, Name, M}),
		    {noreply, State};
		error ->
		    Err = mmd_msg:mkError(M, ?INVALID_CHANNEL,
					  <<"Unknown channel">>),
		    ok = gen_tcp:send(Sock, mmd_encode:encode_message(Err)),
		    {noreply, State}
	    end;
	M=#channel_close{id=Id} ->
	    case chan_pid(State, Id) of
		{ok, Pid} ->
		    case Pid of
			P when is_pid(P) ->
			    erlang:send(Pid, {mmd_tcp_client, Name, M});
			From ->
			    gen_server:reply(From, M)
		    end,
		    {noreply, chan_rm(State, Id)};
		error ->
		    {noreply, State}
	    end
    end;
handle_info({tcp_close, _Sock}, State) ->
    {stop, lost_tcp_connection, State};
handle_info({'DOWN', _Ref, process, Pid, Reason}, State=#state{sock=Sock}) ->
    ?linfo("Pid went down, closing all it's connections: pid=~p, reason=~p",
	   [Pid, Reason]),
    State2 =
	lists:foldl(fun (SvcName, StateN) ->
			    %% we don't bother setting up to handle the ack
			    M=#channel_create{
			      id=p6uuid:next(),
			      service = <<"serviceregistry">>,
			      type=$C,
			      body={'$map', [{<<"unregister">>, SvcName}]}},
			    ok=gen_tcp:send(Sock, mmd_encode:encode_message(M)),
			    svc_rm(StateN, SvcName)
		    end, State, pid_svcs(State, Pid)),
    State3 =
	lists:foldl(fun (Id, StateN) ->
			    Err = ?error(?UNEXPECTED_REMOTE_CHANNEL_CLOSE,
					 <<"Unexpected remote channel close">>),
			    M=#channel_close{id=Id, body=Err},
			    ok=gen_tcp:send(Sock, mmd_encode:encode_message(M)),
			    chan_rm(StateN, Id)
		    end, State2, pid_chans(State2, Pid)),
    {noreply, State3};
handle_info(Info, State) ->
    ?lwarn("Unexpected handle_info(~p, ~p)",[Info, State]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
