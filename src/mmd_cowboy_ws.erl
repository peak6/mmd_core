-module(mmd_cowboy_ws).
-behaviour(cowboy_http_handler).
-behaviour(cowboy_http_websocket_handler).
-export([init/3, handle/2, terminate/2]).
-export([websocket_init/3, websocket_handle/3,
	 websocket_info/3, websocket_terminate/3]).

-include("mmd_cowboy_common.hrl").
-record(state,{chans=channel_mgr:new(),trace=true,cfg=#mmd_cfg{}}).

-define(DOWN,{'DOWN',_Ref,process,_Pid,_Reason}).

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

websocket_init(_Any, Req, #htcfg{trace=Trace}) ->
    {ok,_Transport,Socket} = ?get(transport,Req), %% Not sure if this is a stable api
    {Peer,_} = ?get(peer,Req),
    con_tracker:registerConnection(self(),websocket,format_addr(Peer),Socket),
    Req2 = cowboy_http_req:compact(Req),
    {ok, Req2, #state{trace=Trace}, hibernate}.

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
    gen_server:reply(Ref,ok),
    process_mmd(Msg,From,Req,State);

%% Used for general gen_server:call() handling, can not send anything to websocket, must immediately return
websocket_info({'$gen_call',Ref,Msg},Req,State) ->
    case handle_call(Msg,Req,State) of
	{reply,Reply,NewReq,NewState} ->
	    gen_server:reply(Ref,Reply),
	    {ok,NewReq,NewState};
	Other ->
	    ?lerr("Bad return from handle_call(~p,~p,~p)\nResult: ~p",[Msg,Req,State,Other]),
	    {Other,Req,State}
    end;
websocket_info(Down=?DOWN,Req,State=#state{chans=Chans}) ->
    case channel_mgr:process_down(Chans,Down) of
	{ok,NewChans,Messages} ->
	    jsreply(Messages,Req,State),
	    {ok,Req,State#state{chans=NewChans}};
	{error,Other} ->
	    ?ldebug("proc down got: ~p",[Other]),
	    {ok,Req,State,hibernate}
    end;
websocket_info({dispatch,Message},Req,State) ->
    jsreply(Message,Req,State);

%% Catch all to avoid blowing up socket
websocket_info(_Info, Req, State) ->
    ?lerr("Unexepected message: ~p",[_Info]),
    {ok, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

handle_call(trace,Req,State) ->
    ?linfo("Tracing enabled"),
    {reply,{trace,enabled},Req,State#state{trace=true}};
handle_call(notrace,Req,State) ->
    ?linfo("Tracing disabled"),
    {reply,{trace,disabled},Req,State#state{trace=false}};
handle_call(state,Req,State) ->
    {reply,?DUMP_REC(state,State),Req,State};
handle_call(req,Req,State) ->
    {reply,?DUMP_REC(http_req,Req),Req,State};
handle_call(socket,Req,State) ->
    {ok,_Transport,Socket} = ?get(transport,Req),
    {reply,Socket,Req,State};
handle_call(Other,Req,State) ->
    {reply,{error,{unsupported,Other}},Req,State}.

process_mmd(Msg,From,Req,State=#state{chans=Chans}) ->
    case channel_mgr:process_remote(Chans,From,Msg) of
        {NewChans,Dispatch} -> jsreply(Dispatch,Req,State#state{chans=NewChans});
        NewChans -> jsreply([],Req,State#state{chans=NewChans})
    end.

process_ws(#channel_create{id=Id,service='$mmd',body=?map(Map)}, Req,State=#state{cfg=MMDCfg}) ->
    NewMMDCfg = mmd_cfg:update(MMDCfg,lists:map(fun({A,B}) -> {p6str:mkatom(A),B} end,Map)),
    ?trc(State,"Updated MMDCfg: ~w -> ~w",[?DUMP_REC(mmd_cfg,MMDCfg),?DUMP_REC(mmd_cfg,NewMMDCfg)]),
    jsreply(#channel_close{id=Id,body=ok},Req,State#state{cfg=NewMMDCfg});

process_ws(Msg,Req,State=#state{chans=Chans,cfg=Cfg}) ->
    {NewChans,ForMe} = channel_mgr:process_local(Chans,Msg,Cfg),
    jsreply(ForMe,Req,State#state{chans=NewChans}).

jsreply([],Req,State) -> {ok,Req,State,hibernate};
jsreply(Messages,Req,State) when is_list(Messages) ->
    lists:foreach(fun(M)-> self() ! {dispatch,M} end, Messages),
    {noreply,Req,State};
jsreply(Msg,Req,State) ->
    case catch json_encode:encode(Msg) of
        {ok,JSON} ->
	    ?trc(State,"Sending: ~p",[JSON]),
	    {reply,{text,JSON},Req,State,hibernate};
        Error ->
            ?lerr("Error encoding: ~p~nError message: ~p",[Msg,Error]),
	    {reply,{text,mkError(Msg,"Error encoding response, contact support.")},Req,State,hibernate}
    end.

mkError(#channel_create{id=Id},Msg) -> mkError(Id,Msg);
mkError(#channel_close{id=Id},Msg) -> mkError(Id,Msg);
mkError(#channel_message{id=Id},Msg) -> mkError(Id,Msg);
mkError(Id,Msg) -> mkError(Id,?SERVICE_ERROR,Msg).

mkError(Id,Code,Msg) ->
    json:encode(
      {obj,[
            {close,p6str:mkbin(uuid:to_string(Id))},
            {body,{obj,[{<<"_mmd_error">>,Code},{msg,p6str:mkbin(Msg)}]}}
           ]
      }).
