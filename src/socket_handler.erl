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
%%%-------------------------------------------------------------------
%%% @author dbudworth@peak6.com
%%% @copyright (C) 2011, PEAK6 Investments, L.P.
%%% @doc
%%%
%%% @end
%%% Created : Mon Mar 21 10:49:45 CDT 2011
%%%-------------------------------------------------------------------
-module(socket_handler).

-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([takeControl/2]).
-export([getInfo/2]).
-export([trace/1,notrace/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("p6core/include/logger.hrl").
-include("mmd.hrl").
-include("mmd_cfg.hrl").

-define(SERVER, ?MODULE).

-record(state, {socket,
		chans=channel_mgr:new(),
		sockName,
		trace=false,
		ack=disabled,
		mmdCfg=#mmd_cfg{}}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(MaxChansPerSock) ->
    gen_server:start_link(?MODULE, [MaxChansPerSock], []).

trace(Pid) -> call(Pid,trace).
notrace(Pid) -> call(Pid,notrace).

takeControl(HandlerPid,Socket) ->
    ok=gen_tcp:controlling_process(Socket,HandlerPid),
    gen_server:call(HandlerPid,{take,Socket}).

getInfo(Pid,Fields) -> call(Pid,{getInfo,Fields}).

call(Pid,Term) -> gen_server:call(Pid,Term).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([MaxChansPerSock]) ->
    process_flag(trap_exit,true),
    {ok, #state{chans = channel_mgr:new(MaxChansPerSock)}}.

verifySocket(Socket) ->
%    gen_tcp:send(Socket,?CODEC_VERSION),
    case gen_tcp:recv(Socket,0,3000) of
        {ok,?CODEC_VERSION} -> ok;
        {ok,<<?CODEC_MAJOR:8,Other:8>>} ->
            ?linfo("Codec minor version mismatch, my version: ~p, their version: ~p",[?CODEC_MINOR,Other]),
            ok;
        {ok,Other} -> {bad_version,{expected,?CODEC_VERSION},{received,Other}};
        Err -> Err
    end.

handle_call({mmd,_MMDFrom,#channel_create{id=Id}},_From,State=#state{ack=wait}) ->
    ?ldebug("Rejecting due to pending ack : ~p",[Id]),
    {reply,{error,waiting_ack},State};
handle_call({mmd,From,Msg=#channel_create{}},_From,State=#state{chans=Chans,ack=ready}) ->
    ?linfo("Entering ack wait"),
    case channel_mgr:processIn(Chans,From,Msg) of
        {NewChans,Dispatch} ->
            dispatch(Dispatch,State);
        NewChans -> ok
    end,
    {reply,ok,State#state{chans=NewChans,ack=wait}};

handle_call({mmd,From,Msg},_From,State=#state{chans=Chans}) ->
    case channel_mgr:processIn(Chans,From,Msg) of
        {NewChans,Dispatch} ->
            dispatch(Dispatch,State);
        NewChans -> ok
    end,
    {reply,ok,State#state{chans=NewChans}};

handle_call(trace,_From,State) -> 
    ?linfo("Enabled tracing"),
    {reply,ok,State#state{trace=true}};

handle_call(notrace,_From,State) -> 
    ?linfo("Disabled tracing"),
    {reply,ok,State#state{trace=false}};

handle_call({getInfo,Fields},_From,State) -> 
    {reply,record_infograbber:getInfo(Fields,State,record_info(fields,state)),State};

handle_call({take,Socket}, _From, State) ->
    SName = p6str:sock_to_str(Socket),
    case verifySocket(Socket) of
        ok ->
            inet:setopts(Socket,[{active,once},{recbuf,1024*1024},{sndbuf,1024*1024}]),
            con_tracker:registerConnection(self(),socket,SName,Socket),
            {reply,ok,State#state{socket=Socket,sockName=SName}};
        Other ->
            ?lerr("Failed to negotiate with client: (~s)~p -- ~p",[SName,Socket,Other]),
            {stop,normal,{error,Other},ignore}
    end;


handle_call(Request, From, State) ->
    ?linfo("Unexpected handle_call(~p, ~p, ~p)",[Request,From,State]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    ?linfo("Unexpected handle_cast(~p, ~p)",[Msg,State]),
    {noreply, State}.


handle_info({tcp,_,<<?ACK>>},State=#state{socket=Socket}) ->
    ?linfo("Entering ack ready"),
    inet:setopts(Socket,[{active,once}]),
    {noreply,State#state{ack=ready}};

handle_info({tcp,_,<<?PING>>},State=#state{socket=Socket}) ->
    inet:setopts(Socket,[{active,once}]),
    gen_tcp:send(Socket,<<?PONG>>),
    {noreply,State};
    
handle_info({tcp,_,Data},State=#state{mmdCfg=MMDCfg,socket=Socket,chans=Chans,trace=Trace}) ->
    inet:setopts(Socket,[{active,once}]),
    Msg = mmd_decode:decodeRaw(Data),
    case Trace of
        true -> ?linfo("Received from socket: ~w",[Msg]);
        false -> ok
    end,
    case Msg of 
        #channel_create{id=Id,service=Service,body=?raw(RawMap)} when Service == '$mmd'; Service == <<"$mmd">> ->
            ?map(Map) = mmd_decode:decodeRawFull(RawMap),
            NewMMDCfg = mmd_cfg:update(MMDCfg,lists:map(fun({A,B}) -> {p6str:mkatom(A),B} end,Map)),
            ?linfo("Updated MMDCfg: ~w -> ~w",[MMDCfg,NewMMDCfg]),
            dispatch(#channel_close{id=Id,body=ok},State),
            {noreply,State#state{mmdCfg=NewMMDCfg}};
        _ ->
            case channel_mgr:processOut(Chans,Msg,MMDCfg) of
                {NewChans,Msgs} -> 
                    dispatch(Msgs,State),
                    {noreply,State#state{chans=NewChans}}
            end
    end;

handle_info({tcp_closed,_},_) ->
    {stop,normal,nostate};

handle_info({'EXIT',Pid,Reason},State=#state{chans=Chans}) ->
    ?lwarn("Force closing channels due to '~p' death for: ~p(~p)",[Reason,Pid,node(Pid)]),
    case channel_mgr:refToIds(Chans, Pid) of
        [] -> ?lwarn("Linked process: ~p(~p) exited: ~p, but no channels associated",
                     [Pid,node(Pid),Reason]);
        Items when is_list(Items) ->
            ErrMsg = ?error(?UNEXPECTED_REMOTE_CHANNEL_CLOSE,
                            <<"Connection to the remote channel was lost.">>),
            dispatch([#channel_close{id=Id,body=ErrMsg} || Id <- Items],State)
    end,
    {noreply,State#state{chans=channel_mgr:removeRef(Chans, Pid)}};

handle_info(Info, State) ->
    ?linfo("Unexpected handle_info(~p, ~p)",[Info,State]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
dispatch(Msgs,State) when is_list(Msgs) ->
    lists:foreach(fun(M) -> dispatch(M,State) end, Msgs);

dispatch(Msg, #state{socket=Socket,trace=Trace}) ->
    case Trace of
        true -> 
            ?linfo("Sending to socket: ~w",[Msg]);
        _ -> ok
    end,
    Bin = mmd_encode:encode(Msg),
    case gen_tcp:send(Socket,Bin) of
        {error,timeout} ->
            ?lerr("Socket send timeout occured, exiting"),
            flush(),
            exit({socket_error,send_timeout});
        {error,Other} ->
            ?lerr("Error sending to socket, exiting.  ERROR: ~p",[Other]),
            flush(),
            exit({socket_error,Other});
        ok -> ok
    end.

flush() ->
    receive
        _ -> flush()
    after 0 -> ok
    end.

%% vim: ts=4:sts=4:sw=4:et:sta:
