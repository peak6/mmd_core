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
%%%-------------------------------------------------------------------
%%% @author dbudworth@peak6.com
%%% @copyright (C) 2011, PEAK6 Investments, L.P.
%%% @doc
%%%
%%% @end
%%% Created : Mon Mar 21 11:10:25 CDT 2011
%%%-------------------------------------------------------------------
-module(socket_handlers).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([createHandler/1]).
-export([getConnections/0]).
-include_lib("p6core/include/logger.hrl").

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-define(DEFAULT_RESTART,temporary).
-define(DEFAULT_SHUTDOWN,2000).

-define(CHILD(I, Args, Type), {I, {I, start_link, Args}, ?DEFAULT_RESTART, ?DEFAULT_SHUTDOWN, Type, [I]}).
-define(CHILD(I, Type), {I, {I, start_link, []}, ?DEFAULT_RESTART, ?DEFAULT_SHUTDOWN, Type, [I]}).
%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

getConnections() ->
    lists:map(fun({_,Pid,worker,_}) -> Pid end,supervisor:which_children(?SERVER)).

createHandler(Socket) ->
    Resp = {ok,Pid} = supervisor:start_child(?SERVER, []),
    case socket_handler:takeControl(Pid, Socket) of
        ok -> Resp;
        Other -> Other
    end.


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 1,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Children = [
                ?CHILD(socket_handler,worker)
               ],
    {ok, {SupFlags, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% vim: ts=4:sts=4:sw=4:et:sta:
