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
-module(p6init).

-include("p6init.hrl").

-export([start/0]).
-export([getEnv/0]).
-export([getPaths/1]).

getEnv() ->
    case init:get_argument(p6env) of
        error -> dev;
        {ok,[[Str]]} -> p6str:mkatom(Str)
    end.

getPaths(Base) ->
    Prefix = case filelib:is_dir(Base) of
                 false -> [];
                 true -> [Base]
             end,
    Prefix ++ filelib:wildcard("deps/*/"++Base)++filelib:wildcard("lib/*/"++Base).

start() ->
    p6init_args:process_args(#env{},init:get_arguments()).
