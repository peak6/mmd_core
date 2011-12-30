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
-export([start_link/0]).

-export([accept/2]).  %Need to export so we can reload this code

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("p6core/include/logger.hrl").


-define(SERVER, ?MODULE). 

-record(state, {lsock,acceptor}).

-define(LISTEN_OPTS, 
        [{active,false},
         {keepalive,true},
         {packet,4},
         {reuseaddr,true},
         {send_timeout,30000},
         binary
        ]).

%%%===================================================================
%%% API
%%%===================================================================
start_link() -> start_link(0).
start_link(Port) ->
    gen_server:start_link(?MODULE, [Port], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Port]) -> 
    {ok,LSock} = gen_tcp:listen(Port,?LISTEN_OPTS),
    ?linfo("Listening on: ~s",[p6str:local_sock_to_str(LSock)]),
    Acceptor = proc_lib:spawn_link(fun() -> accept(self(),LSock) end),
    {ok,#state{lsock=LSock,acceptor=Acceptor}}.

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
accept(Server,LSock) ->
    {ok,Sock} = gen_tcp:accept(LSock),
    socket_handlers:createHandler(Sock),
    ?MODULE:accept(Server,LSock).

%% vim: ts=4:sts=4:sw=4:et:sta:
