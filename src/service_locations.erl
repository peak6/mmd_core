-module(service_locations).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-behaviour(gen_server2).
-define(SERVER,?MODULE).
-define(SERVICE,'service.locations').

-include_lib("p6core/include/p6core.hrl").
-include("mmd.hrl").

%%%===================================================================
%%% gen_server2 callbacks
%%%===================================================================
start_link() ->
    gen_server:start_link({local,?SERVER},?MODULE,[],[]).


init([]) ->
    services:regGlobal(?SERVICE),
    {ok,nostate}.

handle_call({mmd,From,Msg=#channel_create{type=call}},_From,State) ->
    mmd_msg:reply(From,Msg,?map(services:service2Nodes())),
    {reply,ok,State};

handle_call({mmd,From,Msg=#channel_create{type=sub}},_From,State) ->
    mmd_msg:error(From,Msg,?INVALID_REQUEST,"Subscribe not supported for: ~s",[?SERVICE]),
    {reply,ok,State};

handle_call(M={mmd,_,#channel_close{}},_From,State) ->
    ?lwarn("Should have never received this: ~p",[M]),
    {reply,ok,State};

handle_call(Request, From, State) ->
    ?lwarn("Unexpected handle_call(~p, ~p, ~p)",[Request,From,State]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    ?lwarn("Unexpected handle_cast(~p, ~p)",[Msg,State]),
    {noreply, State}.

handle_info(Info,State) ->
    ?lwarn("Unexpected handle_info(~p, ~p)",[Info,State]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

