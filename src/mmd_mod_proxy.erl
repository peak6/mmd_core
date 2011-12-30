-module(mmd_mod_proxy).


-behaviour(gen_server).

%% API
-export([start_link/2,getState/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% if we are running tests, export all
-ifdef(TEST).
-compile(export_all).
-endif.

-include_lib("p6core/include/p6core.hrl").
-include_lib("mmd_core/include/mmd.hrl").

-record(state, {created=0,chans=[],mod}).
-record(chan,{id,srv,cli,srvRef,cliRef}).

-define(DOWN(Ref,Pid,Reason), {'DOWN',Ref,process,Pid,Reason}).

getState(Module) ->
    Name = p6str:mkatom("~s_mod_proxy",[Module]),
    gen_server:call(Name,state).

start_link(Module,Names) ->
    Name = p6str:mkatom("~s_mod_proxy",[Module]),
    gen_server:start_link({local, Name}, ?MODULE, [Module,Name,Names], []).

init([Module,PName,Names]) ->
    ?linfo("Created proxy: ~p (~p)",[Module,PName]),
    lists:foreach(fun({l,Name}) -> services:regLocal(Name);
                     ({_,Name}) -> services:regGlobal(Name);
                     (Name) -> services:regGlobal(Name)
                  end, Names),
    {ok, #state{mod=Module}}.

handle_call(state,_From,State) -> {reply,?DUMP_REC(state,State),State};

handle_call({mmd,From,Msg}, _From, State) ->
    {reply,ok,process(From,Msg,State)};

handle_call(Request, From, State) ->
    ?lwarn("Unexpected handle_call(~p, ~p, ~p)",[Request,From,State]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    ?lwarn("Unexpected handle_cast(~p, ~p)",[Msg,State]),
    {noreply,State}.


handle_info({mmd,From,Msg}, State) ->
    {noreply,process(From,Msg,State)};

handle_info(D=?DOWN(Ref,Pid,Reason),State=#state{chans=Chans}) ->
    ?linfo("Received: ~p",[D]),
    {Who,Chan} = case lists:keyfind(Ref,#chan.cliRef,Chans) of
                     false -> case lists:keyfind(Ref,#chan.srvRef,Chans) of
                                  false -> {unknown,undefined};
                                  R -> {server,R}
                              end;
                     R -> {client,R}
                 end,
    case Reason of
        normal -> ?lwarn("Unexpected ~p exit from: ~p",[Who,Pid]),
                  notifyExit(Ref,Chan);
        _ -> ?lerr("~p: ~p crashed: ~p",[Who,Pid,Reason]),
             notifyExit(Ref,Chan)
    end,
    case Chan of 
        undefined -> {noreply,State};
        _ -> {noreply,State#state{chans=lists:keydelete(Chan#chan.id,#chan.id,Chans)}}
    end;

handle_info(Info, State) ->
    ?lwarn("Unexpected handle_info(~p, ~p)",[Info,State]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
modLoop(Mod,Parent,CC=#channel_create{type=call}) ->
    Mod:handleCall(Parent,mmd_decode:decodeFull(CC)),
    modLoop(Mod,Parent);

modLoop(Mod,Parent,CC=#channel_create{type=sub}) ->
    Mod:handleSubscribe(Parent,mmd_decode:decodeFull(CC)),
    modLoop(Mod,Parent).

modLoop(Mod,Parent) ->
    receive
        M=#channel_close{} -> Mod:handleClose(Parent,M);
        M=#channel_message{} -> Mod:handleMessage(Parent,M),
                                modLoop(Mod,Parent);
        die -> ok;
        Other -> exit({unexpected,Other})
    end.

process(From,CC=#channel_create{id=Id},State=#state{mod=Mod,chans=Chans,created=Created}) ->
    case lists:keyfind(Id,#chan.id,Chans) of
        false ->
            Me = self(),
            Pid = proc_lib:spawn(fun() -> modLoop(Mod,Me,CC) end),
            CliRef = erlang:monitor(process,From),
            SrvRef = erlang:monitor(process,Pid),
            State#state{chans=[#chan{id=Id,srv=Pid,cli=From,srvRef=SrvRef,cliRef=CliRef}|Chans],
                        created=Created+1};
        Chan ->
            ?lwarn("Duplicate channel detected from: ~p/~p, dup of: ~p",[From,CC,?DUMP_REC(chan,Chan)]),
            mmd_msg:error(From,CC,?INVALID_CHANNEL,<<"Duplicate channel detected">>),
            State
    end;

process(From,Msg=#channel_message{id=Id},State=#state{chans=Chans}) ->
    forward(From,Id,Msg,Chans),
    State;

process(From,Msg=#channel_close{id=Id,body=B},State=#state{chans=Chans}) ->
    case B of
        ?error(?TIMEOUT,_) -> 
            ?linfo("Received timeout error from: ~p for ~p",[From,Id]);
        _ -> ok
    end,
    forward(From,Id,Msg,Chans),
    cancelMonitors(Id,Chans),
    State#state{chans=lists:keydelete(Id,#chan.id,Chans)}.

cancelMonitors(Id,Chans) ->
    case lists:keyfind(Id,#chan.id,Chans) of 
        false -> ok; %%?lerr("Can't cancel monitors for unknown channel: ~p",[Id]);
        #chan{srvRef=S,cliRef=C,srv=SrvPid} ->
            erlang:demonitor(S,[flush]),
            erlang:demonitor(C,[flush]),
            SrvPid ! die
    end.

forward(From,Id,Msg,Chans) ->
    case lists:keyfind(Id,#chan.id,Chans) of
        false -> ?lerr("Unknown channel (timeout?): ~p",[Id]);
        #chan{cli=Client,srv=Server} when Client == From -> Server ! mmd_decode:decodeFull(Msg);
        #chan{cli=Client,srv=Server} when Server == From -> mmd_msg:dispatch(self(),Client,Msg);
        #chan{cli=Client,srv=Server} -> ?lwarn("Received response to: ~p from wrong source (~p), expected client: ~p or server: ~p",[Id,From,Client,Server])
    end.

notifyExit(_Ref,undefined) -> ?linfo("Ignoring"),ok;
notifyExit(CliRef,#chan{id=Id,srv=S,srvRef=SrvRef,cliRef=CliRef}) ->
    erlang:demonitor(SrvRef,[flush]),
    S ! mmd_msg:mkError(Id,?UNEXPECTED_REMOTE_CHANNEL_CLOSE,<<"Unexpected remote channel close">>);
notifyExit(SrvRef,#chan{id=Id,cli=C,srvRef=SrvRef,cliRef=CliRef}) ->
    mmd_msg:error(C,Id,?SERVICE_ERROR,<<"Service Error">>),
    erlang:demonitor(CliRef,[flush]).
