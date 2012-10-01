-module(mmd_cm_direct).
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
-export([start_link/0]).

-export([send/2]).
-export([get_socket/1]).

-export([get_pool/1, get_pool_cons/1]).
-export([all_ports/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("mmd.hrl").

-define(SERVER, ?MODULE).

-record(state,{pools=[]}).

-define(MAP,mmd_cm_direct_map).
-define(TABLE,mmd_cm_direct_pools).

all_ports() -> ets:tab2list(?MAP).


send(Pid,Data) when is_pid(Pid) -> send(node(Pid),Data);
send(Node,Data) when is_binary(Data) -> 
    case get_socket(Node) of
        {ok,Socket} -> 
	    {ok,Opts} = inet:getopts(Socket,[sndbuf,recbuf,buffer]),
	    {ok,Stats} = inet:getstat(Socket),
	    ?ldebug("Sending: ~p bytes to ~p, opts: ~p, stats: ~p",[size(Data),Node,Opts,Stats]),
            case gen_tcp:send(Socket,Data) of
		ok -> ?ldebug("Done sending to: ~p",[Node]);
		{error,closed} -> send(Node,Data);
		Other -> Other
	    end;
        Other -> Other
    end;

send(Node,Term) ->
    send(Node,term_to_binary(Term)).
    

    
get_socket(Node) -> 
    case get_pool(Node) of
        {ok,Pool} -> mmd_cm_direct_pool:get_socket(Pool);
        Other -> Other
    end.

get_pool_cons(Node) ->
    case get_pool(Node) of
        {ok,Pid} -> mmd_cm_direct_pool:get_pool(Pid);
        Other -> Other
    end.

get_pool(Node) when Node == node() -> undefined;
get_pool(Node) when is_atom(Node) -> 
    case ets:lookup(?TABLE,Node) of
        [] -> call({new,Node});
        [{_,Pool}] -> {ok,Pool}
    end.

start_link() ->
    gen_server:start_link({local,?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    p6dmap:ensure(?MAP),
    {ok,Name} = inet:gethostname(),
    Port = ranch:get_port(cm_direct),
    p6dmap:addGlobal(?MAP,node(),{Name,Port}),
    ?linfo("Listening for channel_message direct connections on: ~s:~p",
           [Name,Port]),
    ets:new(?TABLE,[named_table,protected]),
    {ok,#state{}}.

handle_call({new,Node}, _From, State) ->
    case p6dmap:get(?MAP,Node) of
        [] -> {reply,undefined,State};
        [[_,{Host,Port}]] ->
            {ok,Pid} = mmd_cm_direct_pool:start_link(Node,Host,Port),
            true = ets:insert_new(?TABLE,{Node,Pid}),
            {reply,{ok,Pid},State}
    end;
        
handle_call(Request, From, State) ->
    ?lwarn("Unexpected handle_call(~p, ~p, ~p)",[Request,From,State]),
    {reply,ok,State}.

handle_cast(Msg, State) ->
    ?lwarn("Unexpected handle_cast(~p, ~p)",[Msg,State]),
    {noreply, State}.

handle_info(Info, State) ->
    ?lwarn("Unexpected handle_info(~p, ~p)",[Info,State]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

call(Term) -> gen_server:call(?SERVER,Term).            
%% vim: ts=4:sts=4:sw=4:et:sta:
