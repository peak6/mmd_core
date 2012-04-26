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
-module(mmd_service_sup).

-behaviour(supervisor).

%% API
-export([start_link/2]).
-export([add_worker/3]).
-export([init/1]).

add_worker(Name,From,Msg) ->
    supervisor:start_child(Name,[From,Msg]).

start_link(Name,Mod) ->
    supervisor:start_link({local, Name}, ?MODULE, [Mod]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Mod]) ->
    {ok, { {simple_one_for_one, 5, 10}, [
					{ignored,
					 {mmd_service,start_link,[Mod]},
					 temporary,
					5000,
					worker,
					[mmd_service]
					}
					]}}.
