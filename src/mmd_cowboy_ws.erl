-module(mmd_cowboy_ws).
-behaviour(cowboy_http_handler).
-behaviour(cowboy_http_websocket_handler).
-export([init/3, handle/2, terminate/2]).
-export([websocket_init/3, websocket_handle/3,
	 websocket_info/3, websocket_terminate/3]).

-include("mmd_cowboy_common.hrl").
-record(state,{chans=channel_mgr:new(),trace=true,cfg=#mmd_cfg{},type=binary}).

-define(DOWN,{'DOWN',_Ref,process,_Pid,_Reason}).

-define(gsreply(Ref,Msg), gen_server:reply(Ref,Msg)).
-define(trc(State,Msg,Args), case State#state.trace of
				 true -> ?ldebug(Msg,Args);
				 _ -> ok
			     end).

init({_Any, http}, Req, _HttpCfg) ->
    case cowboy_http_req:header('Upgrade', Req) of
	{undefined, Req2} -> {ok, Req2, undefined};
	{<<"websocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket};
	{<<"WebSocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket}
    end.

handle(Req, State) ->
    {Path,_Req} = cowboy_http_req:raw_path(Req),
    {ok, Req2} = cowboy_http_req:reply(401,
				       [],
				       [<<"Access denied, '">>,Path,<<"' only supports WebSocket connections">>],
				       Req),
    {ok, Req2, State}.

terminate(_Req, _State) ->
    ok.

format_addr({IP,Port}) -> p6str:ip_port_to_str(IP,Port).

is_set(true) -> true;
is_set(<<"true">>) -> true;
is_set(_Other) -> false.

websocket_init(_Any, OReq, #htcfg{trace=Trace}) ->
    {Props,Req} = cowboy_http_req:qs_vals(OReq),
    RespType = case is_set(p6props:get(<<"binary">>,Props)) of
		   false -> text;
		   true -> binary
	       end,
    {ok,_Transport,Socket} = ?get(transport,Req), %% Not sure if this is a stable api
    {Peer,_} = ?get(peer,Req),
    con_tracker:registerConnection(self(),websocket,format_addr(Peer),Socket,<<"websocket">>, json),
    Req2 = cowboy_http_req:compact(Req),
    {ok, Req2, #state{type=RespType,trace=Trace}, hibernate}.

websocket_handle({text, Data}, Req, State) ->
    ?trc(State,"Received(~p): ~p",[size(Data),Data]),
    case catch json_decode:decode(Data) of
	{'EXIT',{Reason,Stack}} ->
	    ?lerr("Error: ~p / ~p, processing: ~p",[Reason,Stack,Data]),
	    {reply, {text, json:encode({obj,[{'_sock_error',p6str:mkio({Reason,Stack})}]})}, Req, State, hibernate};
	Err = {error,_} ->
	    ?lwarn("Received unparsable json text, error: ~p -- text: ~p",[Err,Data]),
	    {reply,{text,json:encode({obj,[{'_sock_error',p6str:mkio(Err)}]})},Req,State,hibernate};
	Msg ->
	    process_ws(Msg,Req,State)
    end.

%% Used for routing incoming MMD network messages back to websocket client
websocket_info({'$gen_call',Ref,{mmd,From,Msg}},Req,State) ->
    ?gsreply(Ref,ok),
    process_mmd(Msg,From,Req,State);

%% Used for general gen_server:call() handling
websocket_info({'$gen_call',Ref,Msg},Req,State) ->
    handle_call(Msg,Ref,Req,State);

websocket_info(Down=?DOWN,Req,State=#state{chans=Chans}) ->
    ?ldebug("Chans: ~p",[Chans]),
    case channel_mgr:process_down(Chans,Down) of
	{ok,NewChans,Messages} ->
	    reply(Messages,Req,State),
	    {ok,Req,State#state{chans=NewChans}};
	{error,Other} ->
	    ?ldebug("proc down got: ~p",[Other]),
	    {ok,Req,State,hibernate}
    end;
websocket_info({dispatch,Message},Req,State) ->
    ws_reply(Message,Req,State); %% {dispatch,Msg} messages are only used with websockets


%% Catch all to avoid blowing up socket
websocket_info(_Info, Req, State) ->
    ?lerr("Unexepected message: ~p",[_Info]),
    {ok, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

handle_call(trace,Ref,Req,State) ->
    ?linfo("Tracing enabled"),
    ?gsreply(Ref,{trace,enabled}),
    {ok,Req,State#state{trace=true}};
handle_call(notrace,Ref,Req,State) ->
    ?linfo("Tracing disabled"),
    ?gsreply(Ref,{trace,enabled}),
    {ok,Req,State=#state{trace=false}};
handle_call(state,Ref,Req,State) ->
    ?gsreply(Ref,?DUMP_REC(state,State)),
    {ok,Req,State};
handle_call(req,Ref,Req,State) ->
    ?gsreply(Ref,?DUMP_REC(http_req,Req)),
    {ok,Req,State};
handle_call(socket,Ref,Req,State) ->
    {ok,_Transport,Socket} = ?get(transport,Req),
    ?gsreply(Ref,Socket),
    {ok,Req,State};
handle_call(Other,Ref,Req,State) ->
    ?lwarn("Got request: ~p, when state is: ~p",[Other,?DUMP_REC(state,State)]),
    ?gsreply(Ref,{error,{bad_call,Other}}),
    {ok,Req,State}.

process_mmd(Msg,From,Req,State=#state{chans=Chans}) ->
    case channel_mgr:process_remote(Chans,From,Msg) of
        {NewChans,Dispatch} -> reply(Dispatch,Req,State#state{chans=NewChans});
        NewChans -> reply([],Req,State#state{chans=NewChans})
    end.

process_ws(#channel_create{id=Id,service='$mmd',body=?map(Map)}, Req,State=#state{cfg=MMDCfg}) ->
    NewMMDCfg = mmd_cfg:update(MMDCfg,lists:map(fun({A,B}) -> {p6str:mkatom(A),B} end,Map)),
    ?trc(State,"Updated MMDCfg: ~w -> ~w",[?DUMP_REC(mmd_cfg,MMDCfg),?DUMP_REC(mmd_cfg,NewMMDCfg)]),
    reply(#channel_close{id=Id,body=ok},Req,State#state{cfg=NewMMDCfg});

process_ws(Msg,Req,State=#state{chans=Chans,cfg=Cfg}) ->
    {NewChans,ForMe} = channel_mgr:process_local(Chans,Msg,Cfg),
    reply(ForMe,Req,State#state{chans=NewChans}).

reply([], Req, State) -> {ok,Req,State,hibernate};
reply(Msgs,Req,State) -> ws_reply(Msgs,Req,State).

%% Single message in a list, don't queue
ws_reply([Msg],Req,State) -> ws_reply(Msg,Req,State);

%% List of messages, dispatch first and queue rest
%% TODO: See if you can send multiple websocket messages at a time
ws_reply([Msg|Rest],Req,State) -> %% List 'o messages, dispatch 1 and queue rest
    Me = self(),
    lists:foreach(fun(M)-> Me ! {dispatch,M} end, Rest),
    ws_reply(Msg,Req,State);

%%Single message
ws_reply(Msg,Req,State=#state{type=Type}) ->
    {reply,{Type,encode(Msg,Type)},Req,State,hibernate}.


encode(Msg,binary) -> mmd_encode:encode(Msg);
encode(Msg,text) -> okget:ok(json_encode:encode(Msg)).


