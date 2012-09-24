-module(sc).
-include("mmd.hrl").
-export([nextMsg/1,allMsgs/1]).
-export([sub/2,sub/3]).
-export([call/2,call/3,call/4]).
-export([connect/0,connect/1]).
-export([close/1,close/3]).
-export([reply/3]).
-compile([export_all]).

sub(Svc,Body) ->
    {ok,Socket} = connect(),
    {ok,Chan} = sub(Socket,Svc,Body),
    {ok,Socket,Chan}.

sub(Sock,Svc,Body) ->
    CC = #channel_create{type=sub,service=Svc,body=Body},
    send(Sock,CC),
    {ok,CC#channel_create.id}.

reply(Sock,Id,Body) ->
    send(Sock,#channel_message{id=Id,body=Body}).

close(Sock) when is_port(Sock) -> gen_tcp:close(Sock).

close(Sock,Id, Body) ->
    send(Sock,#channel_close{id=Id,body=Body}).

call(Sock,Svc,Body) ->
    call(Sock,Svc,?NO_AUTH,Body).
call(Sock,Svc,AT,Body) ->
    send(Sock,#channel_create{type=call,service=Svc,auth_token=AT,body=Body,timeout=1000}),
    case nextMsg(Sock,1000) of
        {ok,Msg} -> Msg;
        Other -> Other
    end.

call(Svc,Body) ->
   doCall(?NO_AUTH,Svc,Body).

callAT(AT,Svc,Body) ->
    doCall(AT,Svc,Body).

doCall(AT,Svc,Body) ->
    {ok,Sock} = connect(9999),
    Ret = call(Sock,Svc,AT,Body),
    gen_tcp:close(Sock),
    Ret.

allMsgs(Socket) ->
    allMsgs(Socket,[]).
allMsgs(Socket,Acc) ->
    case nextMsg(Socket,0) of
        {ok,Msg} -> allMsgs(Socket,[Msg|Acc]);
        _Other -> lists:reverse(Acc)
    end.

nextMsg(Socket) -> nextMsg(Socket,0).
nextMsg(Socket,Timeout) ->
    case gen_tcp:recv(Socket,0,Timeout) of
        {ok,Packet} -> {ok,mmd_decode:decodeRawFull(Packet)};
        Other -> Other
    end.

register(ServiceName) ->
    {ok,P} = connect(),
    sc:call(P,serviceregistry,p6str:to_lower_bin(ServiceName)),
    timer:sleep(500),
    io:format("~p\n",[sc:allMsgs(P)]),
    {ok,P}.

ack(Sock) -> gen_tcp:send(Sock,<<?ACK>>).

connect() -> connect(9999).

connect(Port) ->
    {ok,Sock} = gen_tcp:connect(localhost,Port,[{packet,4},binary,{active,false}]),
    ok=gen_tcp:send(Sock, ?latest_vsn_bin),
    {ok,Sock}.

send(Socket,Msg) ->
    gen_tcp:send(Socket, mmd_encode:encode_message(Msg)).

%% vim: ts=4:sts=4:sw=4:et:sta:
