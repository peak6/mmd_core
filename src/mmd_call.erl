-module(mmd_call).
-export([call/1,call/2,call/3,call/4]).
-include("mmd.hrl").

-define(DOWN(Ref,Pid,Reason), {'DOWN',Ref,process,Pid,Reason}).


call(Service,Body) -> call(Service,Body,5000).
call(Service,Body,Timeout) -> call(Service,Body,Timeout,?NO_AUTH).
call(Service,Body,Timeout,AuthToken) ->
    call(#channel_create{service=Service,timeout=Timeout,auth_token=AuthToken,body=Body,type=call}).


call(CC=#channel_create{id=Chan,timeout=Timeout}) ->
    Chan = CC#channel_create.id,
    {ok,CCPid} = client_channel:new(self(),CC),
    Ref = erlang:monitor(process,CCPid),
    Resp = receive
               {'$gen_call',GSRef,{mmd,CCPid,#channel_close{id=Chan,body=Body}}} ->
                   gen_server:reply(GSRef,ok),
                   {ok,Body};
               ?DOWN(Ref,CCPid,Reason) ->
                   {error,Reason}
           after Timeout ->
                   {error,timeout}
           end,
    erlang:demonitor(Ref,[flush]),
    Resp.

