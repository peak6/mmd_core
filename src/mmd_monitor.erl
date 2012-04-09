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
-module(mmd_monitor).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([enable/0,enable/1]).
-export([disable/0,disable/1]).
-export([setMonitor/1]).

-export([getMonitors/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("p6core/include/p6core.hrl").

-define(SERVER, ?MODULE).

-record(monitor,{name,listMFA,stat,warn,alert,enabled=true}).

-define(DEFAULT_MONITORS,[
                          #monitor{name=chan_queue,
                                   listMFA={mmd,clientChannels,[]},
                                   stat=message_queue_len,
                                   warn=100,
                                   alert=1000
                                  },
                          #monitor{name=chan_mem,
                                   listMFA={mmd,clientChannels,[]},
                                   stat={total_heap_size,words,mb},
                                   warn=100,
                                   alert=1024},
                          #monitor{name=con_queue,
                                   listMFA={mmd,connections,[]},
                                   stat=message_queue_len,
                                   warn=100,
                                   alert=1000
                                  },
                          #monitor{name=con_mem,
                                   listMFA={mmd,connections,[]},
                                   stat={total_heap_size,words,mb},
                                   warn=100,
                                   alert=1024}
                          ]).
-record(state, {enabled=true, monitors = ?DEFAULT_MONITORS}).

%%%===================================================================
%%% API
%%%===================================================================

setMonitor(M=#monitor{}) -> call({store,M}).

enable() -> call(enable).
disable() -> call(disable).

enable(Name) -> call({enable,Name}).
disable(Name) -> call({disable,Name}).

getMonitors() -> call(getMonitors).

start_link() ->
    gen_server:start_link({local,?MODULE},?MODULE, [], []).


init([]) ->
    timer:send_interval(5000,check),
    {ok, #state{}}.

setEnabled(Name,Monitors,Enabled) ->
    case lists:keyfind(Name,#monitor.name,Monitors) of
        false -> {{error,{not_found,Name}},Monitors};
        M=#monitor{} ->{ok,lists:keyreplace(Name,#monitor.name,Monitors,M#monitor{enabled=Enabled})}
    end.

handle_call({store,M=#monitor{name=N}},_From,State=#state{monitors=Mons}) ->
    {reply,ok,State#state{monitors=lists:keystore(N,#monitor.name,Mons,M)}};
handle_call(getMonitors,_From,State) -> {reply,State#state.monitors,State};
handle_call(enable,_From,State) -> {reply,ok,State#state{enabled=true}};
handle_call(disable,_From,State) -> {reply,ok,State#state{enabled=false}};
handle_call({enable,Name},_From,State=#state{monitors=Monitors}) ->
    {R,M} = setEnabled(Name,Monitors,true),
    {reply,R,State#state{monitors=M}};
handle_call({disable,Name},_From,State=#state{monitors=Monitors}) ->
    {R,M} = setEnabled(Name,Monitors,false),
    {reply,R,State#state{monitors=M}};

handle_call(Request, From, State) ->
    ?linfo("Unexpected handle_call(~p, ~p, ~p)",[Request,From,State]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    ?linfo("Unexpected handle_cast(~p, ~p)",[Msg,State]),
    {noreply, State}.



handle_info(check, State=#state{enabled=false}) -> {noreply,State};
handle_info(check,State=#state{monitors=Mons}) ->
    runChecks(Mons),
    {noreply,State};

handle_info(Info, State) ->
    ?linfo("Unexpected handle_info(~p, ~p)",[Info,State]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


runChecks(Monitors) when is_list(Monitors) ->
    lists:foreach(fun(I)->runCheck(I) end, Monitors).

info(Tuple,Stat) when is_tuple(Tuple) ->
    p6proc:info(element(1,Tuple),Stat);
info(Pid,Stat) ->
    p6proc:info(Pid,Stat).

doApply({M,F,A}) -> erlang:apply(M,F,A).

runCheck(#monitor{name=Name,listMFA=List,stat=Check,warn=Warn,alert=Alert,enabled=true}) ->
    lists:foreach(
      fun(Item = {_,{_,Val}}) ->
              case Val of
                  X when X >= Alert ->
                      ?lerr("ALERT: ~s exceeded: ~p -- ~w",[Name,Alert,Item]);
                  X when X > Warn ->
                      ?lwarn("~s exceeded warning level: ~p -- ~w",[Name,Warn,Item]);
                  _X -> ok
              end;
         ({_,undefined}) -> ok;
         (undefined) -> ok %% Not sure what generates this, but it was failing in prod
      end,
      [ {P,info(P,Check)} || P <- doApply(List)]);

runCheck(#monitor{enabled=false}) -> ok.

call(Term) -> gen_server:call(?SERVER,Term).

%% vim: ts=4:sts=4:sw=4:et:sta:
