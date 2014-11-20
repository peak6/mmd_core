-module(start_time).


-behaviour(gen_server).

-include("mmd.hrl").

-export([start_link/0]).
-export([get/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-define(SERVER,?MODULE).


get() ->
    gen_server:call(?SERVER, get).

start_link() ->
    gen_server:start_link({local,?SERVER},?MODULE,[],[]).

init([]) ->
    {ok,p6time:nowAs(sec)}.


handle_call(get, _From, State) ->
    {reply, State, State};
handle_call(Request, From, State) ->
    ?lwarn("Unexpected handle_call(~p, ~p, ~p)",[Request,From,State]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    ?lwarn("Unexpected handle_cast(~p, ~p)",[Msg,State]),
    {noreply, State}.

handle_info(Msg, State) ->
    ?lwarn("Unexpected handle_info(~p, ~p)",[Msg,State]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
