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
-module(p6uuid_mgr).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([next/0,next/1]).
-export([verify/0]).
-export([size/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("logger.hrl").

-define(SERVER, ?MODULE).

-define(POOL_SZ,100).

-record(state, {ids=[],size=0}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

next() -> hd(next(1)).
next(N) -> call({next,N}).
size() -> call(size).
verify() -> call(verify).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call(verify,_From,State=#state{size=Sz,ids=Ids}) ->
    {reply,Sz == length(Ids),State};

handle_call(size,_From,State=#state{size=Sz}) ->
    {reply,Sz,State};

handle_call({next,N},_From,State=#state{size=Sz}) when Sz < N ->
    proc_lib:spawn(fun() -> cast({refill,?POOL_SZ,genIds(?POOL_SZ)}) end),
    {reply,genIds(N),State};
handle_call({next,N},_From,State=#state{ids=Ids,size=Sz}) ->
    {Ret,New} = lists:split(N,Ids),
    {reply,Ret,State#state{ids=New,size=Sz-N}};

handle_call(Request, From, State) ->
    ?lwarn("Unexpected handle_call(~p, ~p, ~p)",[Request,From,State]),
    {reply, ok, State}.

handle_cast({refill,NewCount,NewIds},State=#state{ids=Ids,size=Sz}) ->
    {noreply,State#state{size=Sz+NewCount, ids=NewIds++Ids}};

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

%%%===================================================================
%%% Internal functions
%%%===================================================================
genIds(Count) ->
    Cmd = lists:flatten(io_lib:format("uuid -F BIN -n ~p",[Count])),
    case p6exec:exec(Cmd) of
        {0,Bin} -> convert(Bin,Count,[]);
        {Err,Bin} -> ?lwarn("Failed command: ~p~n  Error: ~p, Msg: ~s",[Cmd,Err,Bin]),
                     lists:map(fun(_) -> uuid:srandom() end,lists:seq(1,Count))
    end.

convert(<<>>,0,Acc) -> Acc;
convert(<<Id:16/binary,Rem/binary>>,N,Acc) -> convert(Rem,N-1,[Id|Acc]).

call(Term) -> gen_server:call(?SERVER,Term).
cast(Term) -> gen_server:cast(?SERVER,Term).
