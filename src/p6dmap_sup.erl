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
-module(p6dmap_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([create_dmap/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(DMAP(Name), {Name, {p6dmap_srv, start_link, [Name]}, permanent, 5000, worker, [dmap_srv]}).

%% ===================================================================
%% API functions
%% ===================================================================

create_dmap(Name) ->
    {ok,_} = supervisor:start_child(?MODULE,?DMAP(Name)).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, [
                                 ]} }.

