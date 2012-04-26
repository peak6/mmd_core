-module(mmd_env).
-include("mmd.hrl").
-behavior(mmd_service).

-export([service_call/2]).


service_call(_Client,#channel_create{}) ->
    {reply,p6init:getEnv()}.



