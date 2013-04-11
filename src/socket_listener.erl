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
-module(socket_listener).

-behaviour(gen_server).

%% API
-export([start_link/1]).

-export([accept/2]).  %Need to export so we can reload this code

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("p6core/include/logger.hrl").


-define(SERVER, ?MODULE).

-record(state, {lsock, acceptor}).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Opts) ->
    Filtered = lists:filter(fun(A)->filter_opt(A) end,Opts),
    gen_server:start_link(?MODULE, p6props:put_all(Filtered,listen_opts()), []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
listen_opts() ->
        [{active,false},
         {keepalive,true},
         {packet,4},
         {reuseaddr,true},
         {send_timeout,30000},
	 {recbuf,p6mem:convert(1,mb,bytes)},
	 {sndbuf,p6mem:convert(1,mb,bytes)},
         binary,
	 {nodelay,true},
	 {port,9999},
	 {ip,mmd_bind:ip()}
        ].

filter_opt({sndbuf,_}) -> true;
filter_opt({recbuf,_}) -> true;
filter_opt({port,_}) -> true;
filter_opt({send_timeout,_}) -> true;
filter_opt({keepalive,_}) -> true;
filter_opt({ip,_}) -> true;
filter_opt({nodelay,_}) -> true;
filter_opt(Bad) -> ?lwarn("Ignoring unsupported socket opt: ~p",[Bad]),
		   false.
    

init(ListenOpts) ->
    case gen_tcp:listen(0,ListenOpts) of
	{error,eaddrinuse} -> 
	    ?lerr("Listen address already in use, socket connections not possible. Settings: ~p",[ListenOpts]),
	    {ok,nostate};
	{ok,LSock} -> 
	    ?linfo("Listening on: ~s, options: ~p",[p6str:local_sock_to_str(LSock), ListenOpts]),
	    Acceptor = proc_lib:spawn_link(fun() -> accept(self(), LSock) end),
	    {ok,#state{lsock=LSock, acceptor=Acceptor}}
    end.

handle_call(Request, From, State) ->
    ?linfo("Unexpected handle_call(~p, ~p, ~p)",[Request,From,State]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    ?linfo("Unexpected handle_cast(~p, ~p, ~p)",[Msg,State]),
    {noreply, State}.

handle_info(Info, State) ->
    ?linfo("Unexpected handle_info(~p, ~p)",[Info,State]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
accept(Server, LSock) ->
    {ok,Sock} = gen_tcp:accept(LSock),
    socket_handlers:createHandler(Sock),
    ?MODULE:accept(Server,LSock).

%% vim: ts=4:sts=4:sw=4:et:sta:
