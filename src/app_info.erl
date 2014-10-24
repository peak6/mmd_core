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

-module(app_info).

-include_lib("p6core/include/p6core.hrl").
-include("mmd.hrl").

-export([service_call/2]).

-behaviour(mmd_service).


service_call(Client,CC=#channel_create{}) ->
    
    ?ldebug("service_call(~p,~p)", [Client,CC]),
    
    MMDStatus = case p6init_proc:appStatus("", mmd) of
		    started -> true;
		    _ -> false
		end,
    
    ReleaseLogContents = case file:read_file("RELEASE_LOG") of
			     {ok, Data} -> ?ldebug("Read RELEASE_LOG successfully."),
					   Data;
			     {error, enoent} -> ?ldebug("Failed to read RELEASE_LOG -- file doesn't exist."),
						<<"">>;
			     {error, _} -> ?ldebug("Failed to read RELEASE_LOG.")
			 end,
    
    reply(?map([{releaseLogContents, ReleaseLogContents},
		{startTime, element(2, lists:last(ets:lookup(startup_info, start_time)))},
		{allServicesHaveStarted, MMDStatus}])).

    
reply(<<"fail">>) -> exit({error,asked_to_fail});
reply(Body) -> {reply,Body}.

%% vim: ts=4:sts=4:sw=4:et:sta:
