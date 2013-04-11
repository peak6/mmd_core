-module(random_service).

-export([start_link/0]).
-export([uniform/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-behaviour(gen_server2).
-define(SERVER,?MODULE).


-include_lib("p6core/include/p6core.hrl").
-include("mmd.hrl").

uniform(Max) ->
    gen_server:call(?SERVER,{uniform,Max}).


%%%===================================================================
%%% gen_server2 callbacks
%%%===================================================================
start_link() ->
    gen_server:start_link({local,?SERVER},?MODULE,[],[]).


init([]) ->
    random:seed(now()),
    {ok,nostate}.

handle_call({uniform,0},_From,State) ->
    {reply,0,State};
handle_call({uniform,Max},_From,State) ->
    {reply,random:uniform(Max),State};

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
