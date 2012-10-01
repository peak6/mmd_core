-module(mmd_cm_direct_pool).
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

-behaviour(gen_server).

%% API
-export([start_link/3]).
-export([get_socket/1]).
-export([get_pool/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("mmd.hrl").
-include("mmd_cm.hrl").

-record(state,{name,host,port,pool=[]}).

get_pool(Pid) -> gen_server:call(Pid,get_pool).

get_socket(Pid) -> gen_server:call(Pid,get).

start_link(Node,Host,Port) ->
    gen_server:start_link(?MODULE, [Node,Host,Port], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Node,Host,Port]) ->
    Name = p6str:mkbin("~s (~s:~p)",[Node,Host,Port]),
    ?ldebug("Created pool for: ~s",[Name]),
    {ok,#state{name=Name,host=Host,port=Port}}.

handle_call(get_pool,_From,State=#state{pool=Pool}) ->
    {reply,Pool,State};

handle_call(get, _From, State=#state{pool=[]}) ->
    case new_socket(State) of
        {ok,Socket,NewState} -> {reply,{ok,Socket},NewState}
    end;
handle_call(get, _From, State=#state{pool=[Socket|Rest]}) ->
    {reply,{ok,Socket},State#state{pool=Rest}};

handle_call(Request, From, State) ->
    ?lwarn("Unexpected handle_call(~p, ~p, ~p)",[Request,From,State]),
    {reply,ok,State}.

handle_cast(Msg, State) ->
    ?lwarn("Unexpected handle_cast(~p, ~p)",[Msg,State]),
    {noreply, State}.

handle_info({tcp_closed,Socket},State=#state{pool=Pool,name=Name}) ->
    ?ldebug("Socket: ~p closed, removing from pool: ~s",[Socket,Name]),
    {noreply,State#state{pool=lists:delete(Socket,Pool)}};

handle_info({tcp,Socket,<<>>},State=#state{pool=Pool}) ->
    {noreply,State#state{pool=[Socket|Pool]}};

handle_info(Info, State) ->
    ?lwarn("Unexpected handle_info(~p, ~p)",[Info,State]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

new_socket(State=#state{name=Name,host=Host,port=Port}) ->
    {ok,Socket} = gen_tcp:connect(Host,Port,
                                  [{packet,4},
                                   binary,
                                   {active,true},
				   {nodelay,true},
                                   {sndbuf,?CM_SOCKET_BUFFER_SZ}
                                  ]),
    ?ldebug("Connected: ~s/~p, port: ~s",[Name,Socket,name(Socket)]),
    {ok,Socket,State}.

name(Sock) -> p6str:full_local_sock_bin(Sock).

%% vim: ts=4:sts=4:sw=4:et:sta:
