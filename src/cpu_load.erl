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
-module(cpu_load).

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([util/0,avg1/0,avg5/0,avg15/0]).
-export([util/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include_lib("p6core/include/logger.hrl").
-define(DMAP,cpu_load_map).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

util() -> call(util).
avg1() -> cpu_sup:avg1().
avg5() -> cpu_sup:avg5().
avg15() -> cpu_sup:avg15().

util(Nodes) when is_list(Nodes) -> [{N,L} || [N,_,L] <- p6dmap:any(?DMAP,Nodes)];
util(Node) -> util([Node]).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    p6dmap:new(?DMAP),
    case os:type() of
	{unix,linux}->
	    application:start(os_mon),
	    timer:send_interval(p6props:getApp('cpu_load.updateInterval',1000),update),
	    {ok, genLoad()};
	Unknown->
	    p6dmap:addGlobal(?DMAP,node(),99),
	    ?lwarn("Unsupported os '~p', assuming 99% load",[Unknown]),
	    {ok,99}
    end.

handle_call(util,_From,Load) -> {reply,Load,Load};

handle_call(Request, From, Load) ->
    ?lwarn("Unexpected handle_call(~p, ~p, ~p)",[Request,From,Load]),
    {reply, ok, Load}.

handle_cast(Msg, Load) ->
    ?lwarn("Unexpected handle_cast(~p, ~p)",[Msg,Load]),
    {noreply, Load}.

handle_info(update,_) ->
    {noreply,genLoad()};

handle_info(Info, Load) ->
    ?lwarn("Unexpected handle_info(~p, ~p)",[Info,Load]),
    {noreply, Load}.

terminate(_Reason, _Load) ->
    ok.

code_change(_OldVsn, Load, _Extra) ->
    {ok, Load}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

genLoad() ->
    Load = cpu_sup:util(),
    p6dmap:set(?DMAP,node(),Load),
    Load.

call(Term) -> gen_server:call(?SERVER,Term).
