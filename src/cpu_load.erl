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

-export([util/0,util/1]).

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

util() -> 
    case util(node()) of
	{_,Util} -> Util;
	Other -> Other
    end.
			

util([]) -> [];
util(Nodes) when is_list(Nodes) -> [{N,L} || [N,_,L] <- p6dmap:any(?DMAP,Nodes)];
util(Node) -> case util([Node]) of
		  [] -> undefined;
		  [L] -> L
	      end.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    p6dmap:ensure(?DMAP),
    p6dmap:addGlobal(?DMAP,node(),99),
    case os:type() of
	{unix,linux}->
	    application:start(os_mon),
	    timer:send_interval(p6props:getApp('cpu_load.updateInterval',1000),update);
	Unknown->
	    ?lwarn("Unsupported os '~p', assuming 99% load",[Unknown])
    end,
    {ok,99}.

handle_call(Request, From, Load) ->
    ?lwarn("Unexpected handle_call(~p, ~p, ~p)",[Request,From,Load]),
    {reply, ok, Load}.

handle_cast(Msg, Load) ->
    ?lwarn("Unexpected handle_cast(~p, ~p)",[Msg,Load]),
    {noreply, Load}.

handle_info(update,LastLoad) ->
    {noreply,genLoad(LastLoad)};

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

genLoad(OrigLoad) ->
    case round(cpu_sup:util() * 10) / 10 of
	OrigLoad -> OrigLoad;
	NewLoad -> 
	    p6dmap:set(?DMAP,node(),NewLoad),
	    NewLoad
    end.
