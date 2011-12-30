-module(mmd_env).
-behavior(mmd_service).

-export([handleSubscribe/2,handleCall/2,handleClose/2,handleMessage/2]).

handleSubscribe(From,Msg) -> mmd_msg:notSupported(From,Msg).
handleClose(From,Msg) -> mmd_msg:notSupported(From,Msg).
handleMessage(From,Msg) -> mmd_msg:notSupported(From,Msg).

handleCall(From,Msg) -> mmd_msg:reply(From,Msg,p6init:getEnv()).



