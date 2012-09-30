%%%-------------------------------------------------------------------
%%% @author David Budworth <dbudworth@peak6.com>
%%% @copyright (C) 2011, David Budworth
%%% @doc
%%%
%%% @end
%%% Created : 17 Mar 2011 by David Budworth <dbudworth@peak6.com>
%%%-------------------------------------------------------------------
-module(mmd_core_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-include("mmd.hrl").
%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @spec start(StartType, StartArgs) -> {ok, Pid} |
%%                                      {ok, Pid, State} |
%%                                      {error, Reason}
%%      StartType = normal | {takeover, Node} | {failover, Node}
%%      StartArgs = term()
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    case mmd_core_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    ?lwarn("Shutting down..."),
    init:stop(?EXIT_ERROR),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% vim: ts=4:sts=4:sw=4:et:sta:
