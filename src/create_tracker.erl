-module(create_tracker).

-export([start_link/0]).
-export([incr/1,clear/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-behaviour(gen_server2).
-define(SERVER,?MODULE).
-define(SERVICE,'service.calls').

-define(TABLE,?MODULE).

-include_lib("p6core/include/p6core.hrl").
-include("mmd.hrl").

incr(?SERVICE) -> ok;
incr(Service) -> cast({incr,p6str:mkatom(Service)}).

clear(Service) -> cast({clear,p6str:mkatom(Service)}).


%%%===================================================================
%%% gen_server2 callbacks
%%%===================================================================
start_link() ->
    gen_server:start_link({local,?SERVER},?MODULE,[],[]).


init([]) ->
    ?TABLE = ets:new(?TABLE,[named_table,protected]),
    services:regGlobal(?SERVICE),

    {ok,nostate}.

handle_call({mmd,From,Msg},_From,State) ->
    mmd_msg:reply(From,Msg,?map(ets:tab2list(?TABLE))),
    {reply,ok,State};

handle_call(Request, From, State) ->
    ?lwarn("Unexpected handle_call(~p, ~p, ~p)",[Request,From,State]),
    {reply, ok, State}.

handle_cast({clear,Name},State) ->
    ets:delete(?TABLE,Name),
    {noreply,State};

handle_cast({reg,Name},State) ->
    ins(Name),
    {noreply,State};

handle_cast({incr,Name},State) ->
    case catch inc(Name) of
        {'EXIT',{badarg,_}} ->
            ins(Name),
            inc(Name);
        _ -> ok
    end,
    {noreply,State};

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

cast(Term) -> gen_server2:cast(?SERVER,Term).

inc(Name) ->
    ets:update_counter(?TABLE,Name,1).

ins(Name) ->
    ets:insert_new(?TABLE,{Name,0}).

