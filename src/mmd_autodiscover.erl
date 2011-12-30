%%%-------------------------------------------------------------------
%%% @author David Budworth <dbudworth@pslchi6dngdbus14.peak6.net>
%%% @copyright (C) 2011, David Budworth
%%% @doc
%%%
%%% @end
%%% Created : 16 Jun 2011 by David Budworth <dbudworth@pslchi6dngdbus14.peak6.net>
%%%-------------------------------------------------------------------
-module(mmd_autodiscover).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([ping/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-include_lib("p6core/include/p6core.hrl").

-record(state, {sock,env,addr,port,ping,pingCount=0,nodeCount=0,ignore=[],timer}).
-record(ping,{env,node,cookie}).
-define(BASE_TIMEOUT,4000). %% Means initial standalone ping time is 5 sec BASE+(nodes * 1000)

ping() ->
    ?SERVER ! forcePing.

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    case application:get_env(auto_discover) of
        undefined -> 
            ?linfo("Auto discover disabled"),
            ignore;
        {ok,{Addr,Port}} ->
            Env = p6init:getEnv(),
            Ping = #ping{env=Env,node=node(),cookie=erlang:get_cookie()},
            {ok,Sock} = gen_udp:open(Port,[binary,
                                           {active,true},
                                           {reuseaddr,true},
                                           {multicast_ttl,64},
                                           {add_membership,{Addr,{0,0,0,0}}}
                                          ]),
            {ok,sendPing(#state{env=Env,sock=Sock,port=Port,ping=Ping,addr=Addr})}
    end.

handle_call(Request, From, State) ->
    ?lwarn("Unexpected handle_call(~p, ~p, ~p)",[Request,From,State]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    ?lwarn("Unexpected handle_cast(~p, ~p)",[Msg,State]),
    {noreply,State}.

    
handle_info({udp,Sock,FromIP,Port,Bin},State=#state{sock=Sock,env=MyEnv,port=Port,ignore=Ignore}) ->
    MyNode = node(),
    MyCookie = erlang:get_cookie(),
    case catch binary_to_term(Bin) of
        #ping{node=MyNode} -> {noreply,State}; %% Ignore our own ping
        #ping{env=MyEnv,node=Node,cookie=MyCookie} -> 
            case lists:member(Node,nodes()) of
                true -> {noreply,State};
                false ->
                    ?linfo("Joining: ~s, with cookie: ~p",[Node,erlang:get_cookie()]),
                    mmd:tempJoin(Node),
                    {noreply,adjustTimer(State#state{ignore=[]})}
            end;
        Ping = #ping{} -> 
            case lists:member(Ping,Ignore) of
                true -> {noreply,State};
                false ->
                    ?linfo("Ignoring ping from: ~s, my env: ~s/~p, received: ~p",
                           [p6str:ip_port_to_str(FromIP,Port), MyEnv,MyCookie,?DUMP_REC(ping,Ping)]),
                    {noreply,State#state{ignore=[Ping|Ignore]}}
            end;
        Other -> ?lerr("Ignoring multicast packet: ~p, due to: ~p from: ~s",
                       [Bin,Other,p6str:ip_port_to_str(FromIP,Port)]),
                 {noreply,State}
    end;

handle_info(forcePing,State=#state{pingCount=PC}) ->
    ?linfo("Sending ping: ~p",[PC]),
    {noreply,sendPing(State)};

handle_info(sendPing,State) ->
    {noreply,sendPing(State)};


handle_info(Msg, State={state,Sock,Env,Addr,Port,Ping,PingCount}) ->
    NewState = #state{sock=Sock,env=Env,addr=Addr,port=Port,ping=Ping,pingCount=PingCount,ignore=[]},
    ?linfo("Transformed state from: ~p to: ~p",[State,NewState]),
    handle_info(Msg,NewState);

handle_info(Info, State) ->
    ?lwarn("Unexpected handle_info(~p, ~p)",[Info,State]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

sendPing(State=#state{sock=Sock,addr=Addr,port=Port,ping=Ping,pingCount=PC}) ->
    case PC of
        0 -> ?linfo("Pinging ~p with ~p",[p6str:ip_port_to_str(Addr,Port),Ping]);
        _ -> ok
    end,
    ok = gen_udp:send(Sock,Addr,Port,term_to_binary(Ping)),
    adjustTimer(State#state{pingCount=PC+1}).

adjustTimer(State=#state{timer=T,nodeCount=OrigNC}) ->
    case length(nodes())+1 of
        OrigNC -> State;
        NC ->
            case T of 
                undefined -> ok;
                Ref ->
                    {ok,cancel} = timer:cancel(Ref)
            end,
            Timeout = ?BASE_TIMEOUT+(NC*1000),
            ?linfo("Node count changed (~p), adjusting ping interval to: ~p",[NC,Timeout]),
            {ok,TRef} = timer:send_interval(Timeout,sendPing),
            State#state{timer=TRef,nodeCount=NC}
    end.
            
            
