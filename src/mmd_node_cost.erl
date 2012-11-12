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
-module(mmd_node_cost).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([set_dc_cost/2]).
-export([get_cost/1]).
-export([get_dc/1]).
-export([get_dcs/0]).
-export([get_nodes/0]).
-export([get_dc_cost/1]).
-export([override/2, clear_override/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-compile({no_auto_import,[get/0]}).

-include_lib("p6core/include/logger.hrl").

-define(SERVER, ?MODULE).
-define(TABLE,?SERVER).
-record(node,{node='_',cost='_',dc='_',override='_'}).
-record(dc,{dc='_',cost='_'}).

%%%===================================================================
%%% API
%%%===================================================================
get_cost(Node) -> 
    case ets:match(?TABLE,#node{node=Node,cost='$1'}) of
	[[C]] -> C;
	[] -> calc_node_cost(Node)
    end.

get_dc(Node) ->
    case ets:match(?TABLE,#node{node=Node,dc='$1'}) of
	[[DC]] -> DC;
	[] -> calc_dc(Node)
    end.

get_dcs() ->
    ets:match_object(?TABLE,#dc{}).
get_nodes() ->
    ets:match_object(?TABLE,#node{}).

get_dc_cost(DC) ->
    case ets:match(?TABLE,#dc{dc=DC,cost='$1'}) of
	[[C]] -> C;
	[] -> calc_dc_cost(DC)
    end.
		   
set_dc_cost(DC,Cost) -> cast({update_dc,DC,Cost}).

override(Node,Cost) -> cast({store_node,Node,get_dc(Node),Cost,true}).
clear_override(Node) -> calc_node_cost(Node).


calc_dc_cost(DC) ->
    Cost = 
	case calc_dc(node()) of
	    DC -> 2;
	    _ -> 3
	end,
    set_dc_cost(DC,Cost),
    Cost.

calc_node_cost(Node) ->
    DC = calc_dc(Node), %not calling get_dc since we probably haven't stored Node yet
    Cost = get_dc_cost(DC),
    cast({store_node,Node,DC,Cost,false}),
    Cost.

start_link() -> 
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    ets:new(?TABLE,[named_table,protected,{keypos,2}]),
    ets:insert(?TABLE,my_dc()),
    ets:insert(?TABLE,my_node()),
    net_kernel:monitor_nodes(true),
    {ok, no_state}.

handle_call(Request, From, State) ->
    ?lwarn("Unexpected handle_call(~p, ~p, ~p)",[Request,From,State]),
    {reply,ok,State}.

handle_cast({update_dc,DC,Cost},State) ->
    ets:insert(?TABLE,#dc{dc=DC,cost=Cost}),
    case ets:match(?TABLE,#node{dc=DC,node='$1',override=false}) of
	[] -> ?ldebug("No nodes to update for: ~p",[DC]);
	Nodes when is_list(Nodes) -> 
	    lists:foreach(fun([Node]) -> ets:update_element(?TABLE,Node,{#node.cost,Cost}) end,Nodes)
    end,
    {noreply,State};

handle_cast({store_node,Node,DC,Cost,Over},State) ->
    ets:insert(?TABLE,#node{node=Node,dc=DC,cost=Cost,override=Over}),
    {noreply,State};

handle_cast(Msg, State) ->
    ?lwarn("Unexpected handle_cast(~p, ~p)",[Msg,State]),
    {noreply, State}.

handle_info({nodedown,_Node},State) -> %% Supress node down messages, we only care about nodeup
    {noreply,State};

handle_info({nodeup,Node},State) ->
    get_cost(Node),
    {noreply,State};

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
cast(Args) -> gen_server:cast(?SERVER,Args).

my_node() -> #node{node=node(),dc=node,cost=0,override=false}.
my_dc() -> #dc{dc=calc_dc(node()),cost=1}.

calc_dc(Node) -> mmd:get_dc(Node).

%% vim: ts=4:sts=4:sw=4:et:sta:
