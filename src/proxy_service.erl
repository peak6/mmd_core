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
%%% Created : Tue Jul 07 08:43:30 CDT 2011
%%%-------------------------------------------------------------------

%% The proxy server uses mmd_tcp_client to connect to a remote mmd
%% server and exposes all the locally available services to the remote
%% mmd.
%%
%% To set this up, you'll want to add something like the following to
%% your config (replacing with your own names):
%%
%%   {set, mmd_core,
%%    [
%%     ...
%%     {mmd_tcp_clients, [{remote_mmd, "remote.name.com", 9999}]},
%%     {proxies, [{remote_proxy, remote_mmd}]},
%%     ...
%%
%% Now all the services that are available locally will be exposed on
%% "remote.name.com"'s mmd (and any connected mmd nodes) prefixed with
%% "remote_proxy".
%%
%% For example, you can now do:
%%
%%     > c = pymmd.connect()
%%     > c.remote_proxy.echo("Hello World!")
%%     MMDChannelClose(..., body='Hello World!')

-module(proxy_service).

-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include_lib("p6core/include/logger.hrl").
-include("mmd.hrl").

-record(state, {mmd, known_svcs, mmd_mon_ref, svc_prefix, chans}).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Name, RemoteMmdName) ->
    gen_server:start_link({local, Name}, ?MODULE, {Name, RemoteMmdName}, []).

%%%===================================================================
%%% handler helpers
%%%===================================================================

prefix_name(#state{svc_prefix=Prefix}, Svc) ->
    SvcBin = p6str:mkbin(Svc),
    <<Prefix/binary, SvcBin/binary>>.

strip_prefix(#state{svc_prefix=Prefix}, Svc) ->
    p6str:mkatom(binary:replace(p6str:mkbin(Svc), Prefix, <<>>)).

update_svcs(State=#state{known_svcs=Known, mmd=Mmd}) ->
    All = sets:from_list(services:allServiceNames()),
    lists:foreach(
      fun (Svc) ->
	      mmd_tcp_client:unregister(Mmd, prefix_name(State, Svc))
      end, sets:to_list(sets:subtract(Known, All))),
    lists:foreach(
      fun (Svc) ->
	      mmd_tcp_client:register(Mmd, prefix_name(State, Svc))
      end, sets:to_list(sets:subtract(All, Known))),
    State#state{known_svcs=All}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init({Name, RemoteMmdName}) ->
    ?linfo("Starting mmd proxy ~p for remote mmd ~p", [Name, RemoteMmdName]),
    MmdMonRef = erlang:monitor(process, RemoteMmdName),
    timer:send_interval(500, update_svcs),
    NameBin = p6str:mkbin(Name),
    {ok, #state{mmd=RemoteMmdName,
		known_svcs=sets:new(),
		mmd_mon_ref=MmdMonRef,
		svc_prefix= <<NameBin/binary, ".">>,
		chans=channel_mgr:new()}}.

handle_call({mmd, From, M}, _From, State=#state{chans=Chans, mmd=Mmd}) ->
    NewChans = case channel_mgr:process_remote(Chans, From, M) of
		   {Chans2, _} ->
		       mmd_tcp_client:send(Mmd, M),
		       Chans2;
		   Chans2 -> Chans2
	       end,
    {reply, ok, State#state{chans=NewChans}};

handle_call(Request, From, State) ->
    ?lwarn("Unexpected handle_call(~p, ~p, ~p)",[Request,From,State]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    ?lwarn("Unexpected handle_cast(~p, ~p)",[Msg,State]),
    {noreply, State}.

handle_info({mmd_tcp_client, _Mmd, M}, State=#state{chans=Chans}) ->
    M2 = case M of
	     Msg=#channel_create{service=Svc} ->
		 Msg#channel_create{service=strip_prefix(State, Svc)};
	     Msg -> Msg
	 end,
    NC = case channel_mgr:process_local(Chans, M2) of
	     {NewChans, []} -> NewChans;
	     {NewChans, Other} -> ?linfo("Dunno what to do with: ~p", [Other]),
				  NewChans
	 end,
    {noreply, State#state{chans=NC}};

handle_info(update_svcs, State) ->
    {noreply, update_svcs(State)};
handle_info({'DOWN', Ref, process, _Pid, Reason},
	    State=#state{mmd_mon_ref=MmdMonRef})
  when Ref == MmdMonRef ->
    ?lwarn("We just lost our mmd_tcp_client instance, goodbye. reason: ~p",
	   [Reason]),
    {stop, lost_mmd_tcp_client_instance, State};
handle_info(Info, State) ->
    ?lwarn("Unexpected handle_info(~p, ~p)",[Info, State]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
