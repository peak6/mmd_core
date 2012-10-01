-module(mmd_ranch_cm_direct).
-export([start_link/4, init/4]).

-export([loop/3]).

-include("mmd.hrl").
-define(SOCK_TIMEOUT,30*1000).

start_link(ListenerPid, Socket, Transport, Opts) ->
    Name = p6str:full_remote_sock_bin(Socket),
    Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, [Name|Opts]]),
    ?ldebug("Received connection from: ~s, ~p",[Name,Socket]),
    {ok, Pid}.

init(ListenerPid, Socket, Transport, [Name|_Opts]) ->
    ok = ranch:accept_ack(ListenerPid),
    Transport:setopts(Socket,[{packet,4}]),
    process_flag(priority,high),
    loop(Socket, Name, Transport).

trace(Fmt,Args) ->
    case application:get_env(mmd_core,mmd_cm_trace) of
	{ok,true} -> ?ldebug(Fmt,Args);
	_ -> ok
    end.

loop(Socket, Name, Transport) ->
    case Transport:recv(Socket, 0, ?SOCK_TIMEOUT) of
        {ok, Data} ->
	    trace("Received stuff",[]),
	    {ok,Opts} = inet:getopts(Socket,[sndbuf,recbuf,buffer]),
	    {ok,Stats} = inet:getstat(Socket),
	    trace("Received: ~p bytes, ~p,  opts: ~p, stats: ~p",[size(Data),Name,Opts,Stats]),
            case erlang:binary_to_term(Data) of
                {msg,To,From,Msg} ->
                    To ! {mmd,From,Msg};
                {call,To,From,Msg} ->
                    gen_server:call(To,{mmd,From,Msg})
            end,
            Transport:send(Socket,<<>>),
            ?MODULE:loop(Socket, Name, Transport);
        {error,timeout} ->
            ?ldebug("Shutting down: ~s due to ~p seconds of inactivity",
                    [Name,?SOCK_TIMEOUT/1000]),
            ok = Transport:close(Socket);
        {error,closed} ->
            ?ldebug("Client closed socket: ~s",[Name]),
            ok;
        Reason ->
            ?lerr("Closing socket: ~p, due to: ~p",[Name,Reason]),
            ok = Transport:close(Socket)
    end.
