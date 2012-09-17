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
-module(mmd_node_tags).

-behaviour(gen_server).

%% API
-export([start_link/0,start_link/1]).
-export([add/1, remove/1, get/0,has/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-compile({no_auto_import,[get/0]}).

-include_lib("p6core/include/logger.hrl").

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

add(Tags) -> cast({add,Tags}).
remove(Tags) -> cast({remove,Tags}).
get() -> call(get).
has(undefined) -> true;
has(Tags) -> p6list:contains_all(to_lower_list(Tags),get()).
	
start_link() -> start_link([]).
start_link(Tags) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Tags], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Tags]) ->
    {ok, to_lower_list(Tags)}.

handle_call(get,_From,Tags) -> {reply,Tags,Tags};
handle_call(Request, From, State) ->
    ?lwarn("Unexpected handle_call(~p, ~p, ~p)",[Request,From,State]),
    {reply,ok,State}.

handle_cast({add,Add},Tags) ->
    {noreply,lists:usort(to_lower_list(Add)++Tags)};
handle_cast({remove,Remove},Tags) ->
    {noreply,Tags -- to_lower_list(Remove)};

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
call(Args) -> gen_server:call(?SERVER,Args).
cast(Args) -> gen_server:cast(?SERVER,Args).

to_lower_list(Items) when is_list(Items) ->
    lists:map(fun p6str:to_lower_bin/1,Items);
to_lower_list(Item) -> to_lower_list([Item]).

%% vim: ts=4:sts=4:sw=4:et:sta:
