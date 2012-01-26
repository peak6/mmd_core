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
-module(http_listener).
-include_lib("p6core/include/p6core.hrl").
-include("mmd_http.hrl").
-export([start_link/1]).

start_link(Port) ->
    Path =
        case application:get_env(http_docroot) of
            {ok,P=[X|_]} when is_list(X) -> P;
            {ok,P} when is_list(P) -> [P];
            undefined -> p6init:getPaths("priv/html")
        end,
    lists:foreach(fun(P) ->
                          case filelib:is_dir(P) of
                              true -> ok;
                              false -> ?lerr("Path ~s is not a valid path",[P]),
                                       exit({bad_path,P})
                          end
                  end,
                  Path),
                       
    Proxy = 
        case application:get_env(http_proxy_url) of 
            undefined -> undefined;
            {ok,Val} -> p6str:mkstr(Val)
        end,
    Cfg = #htcfg{port=Port,path=Path,proxy=Proxy},

    MOpts = [{loop, fun(Req) -> http_handler:handleHttp(Cfg,Req) end},
             {ws_loop, fun(Req) -> http_handler:handleWs(Req,Cfg) end},
             {port,Port}],
    ?linfo("Starting http server on port: ~p with config: ~p",[Port,?DUMP_REC(htcfg,Cfg)]),
    misultin:start_link(MOpts).




