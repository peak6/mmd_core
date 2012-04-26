%% Copyright 2011-2012 PEAK6 Investments, L.P.
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
-module(mmd_service_acceptor).

-behaviour(gen_server).

%% API
-export([start_link/2,getState/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% if we are running tests, export all
-ifdef(TEST).
-compile(export_all).
-endif.

-include_lib("p6core/include/p6core.hrl").
-include_lib("mmd_core/include/mmd.hrl").

-record(state, {created=0,mod,super}).
%%-record(chan,{id,srv,cli,srvRef,cliRef}).

-define(DOWN(Ref,Pid,Reason), {'DOWN',Ref,process,Pid,Reason}).

-define(PROXY(Name), p6str:mkatom("mod_proxy_~s",[Name])).
-define(PROXYSUP(Name), p6str:mkatom("mod_proxy_~s_sup",[Name])).

getState(Module) ->
    gen_server:call(?PROXY(Module),state).

start_link(Module,Names) ->
    Name = ?PROXY(Module),
    gen_server:start_link({local, Name}, ?MODULE, [Module,Name,Names], []).

init([Module,PName,Names]) ->
    ?linfo("Created proxy: ~p (~p)",[Module,PName]),
    SupName = ?PROXYSUP(Module),
    mmd_service_sup:start_link(SupName,Module),
    lists:foreach(fun({l,Name}) -> services:regLocal(Name);
                     ({_,Name}) -> services:regGlobal(Name);
                     (Name) -> services:regGlobal(Name)
                  end, Names),
    {ok, #state{mod=Module,super=SupName}}.

handle_call(state,_From,State) -> {reply,?DUMP_REC(state,State),State};

handle_call({mmd,From,CC=#channel_create{}}, _Ref, State=#state{created=Created,super=Sup}) ->
    case mmd_service_sup:add_worker(Sup,From,CC) of
	{ok,Pid} -> {reply,{ok,Pid},State#state{created=Created+1}}
    end;

handle_call(Request, From, State) ->
    ?lwarn("Unexpected handle_call(~p, ~p, ~p)",[Request,From,State]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    ?lwarn("Unexpected handle_cast(~p, ~p)",[Msg,State]),
    {noreply,State}.

handle_info(Info, State) ->
    ?lwarn("Unexpected handle_info(~p, ~p)",[Info,State]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
