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
-define(PING_TIME,30000).  %Ping every 30 seconds
-define(UPDATE_KNOWN,10000).

-include_lib("p6core/include/p6core.hrl").

-record(state, {known_nodes=[],ignore,node,env,cookie,version,sock,addr,port,ping}).
-record(ping,{env,node,cookie,version}).

ping() ->
    ?SERVER ! send_ping.

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    case application:get_env(auto_discover) of
        undefined ->
            ?linfo("Auto discover disabled"),
            ignore;
        {ok,{Addr,Port}} ->
            Env = p6init:getEnv(),
	    {_,_,Vsn} = lists:keyfind(mmd_core,1,application:loaded_applications()),
	    Cookie = erlang:get_cookie(),
            Ping = #ping{env=Env,node=node(),cookie=Cookie,version=Vsn},
	    case gen_udp:open(Port,[binary,
                                           {active,true},
                                           {reuseaddr,true},
                                           {multicast_ttl,64},
                                           {add_membership,{Addr,{0,0,0,0}}}
                                          ]) of
		{ok,Sock} -> 
		    State = #state{node=node(),cookie=Cookie,version=Vsn,ignore=[],env=Env,sock=Sock,port=Port,ping=Ping,addr=Addr},
		    send_ping(State),
		    timer:send_interval(?PING_TIME,send_ping),
		    timer:send_interval(?UPDATE_KNOWN,update_known),
		    {ok,State};
		{error,eaddrinuse} ->
		    ?lwarn("Autodiscover disabled due to mcast address being in use, you are probably on a mac"),
		    {ok,#state{}}
	    end
    end.

handle_call(Request, From, State) ->
    ?lwarn("Unexpected handle_call(~p, ~p, ~p)",[Request,From,State]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    ?lwarn("Unexpected handle_cast(~p, ~p)",[Msg,State]),
    {noreply,State}.

handle_info({udp,Sock,FromIP,Port,Bin},State=#state{sock=Sock,port=Port,ignore=Ignore}) ->
    case parse(Bin) of
	{ok,Ping} -> 
	    case lists:member(Ping,Ignore) of
		true -> {noreply,State};
		false -> {noreply,process(Ping,State)}
	    end;
	{error,Reason} ->
	    IPP = {FromIP,Port},
	    case lists:member(IPP,Ignore) of
		true -> {noreply,State};
		false -> 
		    ?lwarn("Unrecognizable ping packet from: ~s, parse result: ~p",[p6str:ip_port_to_str(FromIP,Port),Reason]),
		    {noreply,ignore(IPP,State)}
	    end
    end;

handle_info(update_known,State) ->
    {noreply,update_known(State)};

handle_info(send_ping,State) ->
    send_ping(State),
    {noreply,State};

handle_info(Info, State) ->
    ?lwarn("Unexpected handle_info(~p, ~p)",[Info,State]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

update_known(State=#state{known_nodes=Known}) ->
    New = lists:foldl(
	    fun(Node,Acc) ->
		    case lists:member(Node,Acc) of
			true -> Acc;
			false -> [Node|Acc]
		    end
	    end, Known, nodes()),
    State#state{known_nodes=New}.

send_ping(#state{sock=Sock,addr=Addr,port=Port,ping=Ping}) ->
    ok = gen_udp:send(Sock,Addr,Port,term_to_binary(Ping)).

ignore(Item,State=#state{ignore=Ignore}) -> 
    State#state{ignore=[Item|Ignore]}.

parse(Bin) ->
    case catch binary_to_term(Bin) of
	P=#ping{} -> {ok,P};
	{'EXIT',{badarg,_}} -> {error,unparsable};
	OtherTerm -> {error,{bad_term,OtherTerm}}
    end.

%% Ignore on Env mismatch
process(Ping=#ping{env=Env,node=Node},State=#state{env=MyEnv}) when Env =/= MyEnv -> 
    ?linfo("Ignoring remote MMD node: ~p due to environment mismatch, theirs: ~p, mine: ~p",[Node,Env,MyEnv]),
    ignore(Ping,State);

%% Ignore on cookie mismatch
process(Ping=#ping{cookie=Cookie,node=Node},State=#state{cookie=MyCookie}) when Cookie =/= MyCookie ->
    ?linfo("Ignoring remote MMD node: ~p due to cookie mismatch, theirs: ~p, mine: ~p",[Node,Cookie,MyCookie]),
    ignore(Ping,State);

%% Version mismatch, we require exact version matches
process(Ping=#ping{version=Version,node=Node},State=#state{version=MyVersion}) when Version =/= MyVersion ->
    ?linfo("Ignoring remote MMD node: ~p due to version mismatch, theirs: ~p, mine: ~p",[Node,Version,MyVersion]),
    ignore(Ping,State);

%% We saw our own ping
process(#ping{node=MyNode},State=#state{node=MyNode}) -> 
    State;

%% I have a ping and none of the previous patterns matched, so we're good
process(Ping=#ping{node=Node},State) ->
    case lists:member(Node,nodes()) of 
	true -> 
	    State;
	false ->
	    ?ldebug("Received ping: ~p, connecting",[Ping]),
	    mmd:join(Node),
	    State
    end;

process(Other,State) ->
    ?ldebug("Unrecognized ping, adding: ~p to ignore list",[Other]),
    ignore(Other,State).

