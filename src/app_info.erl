-module(app_info).

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
		{startTime, start_time:get()},
		{allServicesHaveStarted, MMDStatus}])).

    
reply(Body) -> {reply,Body}.

%% vim: ts=4:sts=4:sw=4:et:sta:
