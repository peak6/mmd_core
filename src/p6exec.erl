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
-module(p6exec).

-export([exec/1,exec/2]).
-export([execArgs/2,execArgs/3]).


exec(Cmd) -> exec(1000,Cmd).
exec(Timeout, Cmd) when is_integer(Timeout) ->
    catch
        getResponse(Timeout,
                erlang:open_port(
                  {spawn,Cmd},
                  [in,
                   binary,
                   exit_status,
                   stream,
                   stderr_to_stdout])).

execArgs(Cmd,Args) -> execArgs(1000,Cmd,Args).
execArgs(Timeout,Cmd,Args) when is_integer(Timeout) ->
    getResponse(Timeout,
                erlang:open_port(
                  {spawn_executable,os:find_executable(Cmd)},
                  [in,
                   binary,
                   exit_status,
                   stream,
                   stderr_to_stdout,
                   {args,Args}])).

getResponse(Timeout,Port) -> getResponse(p6time:nowAs(ms)+Timeout,Port,<<>>).

getResponse(Timeout,Port,Acc) ->
    Wait = max(0,Timeout - p6time:nowAs(ms)),
    receive
        {Port,{data,Bin}} -> getResponse(Timeout,Port,<<Acc/binary,Bin/binary>>);
        {Port,{exit_status,N}} -> {N,Acc}
    after Wait ->
            erlang:port_close(Port),
            {timeout,Acc}
    end.

