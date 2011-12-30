-module(security_id_cache).

-export([start_link/0]).
-export([stats/0]).

-export([getById/1,getByKey/1]).
-export([store/2]).


-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-behaviour(gen_server2).

-define(SERVER,?MODULE).
-define(ID_TO_KEY,security_id_to_key).
-define(KEY_TO_ID,security_key_to_id).

-include_lib("p6core/include/p6core.hrl").

stats() -> call(stats).

getById(SecId) ->
    case ets:lookup(?ID_TO_KEY,SecId) of
        [{_,Key}] -> cast(hit), Key;
        [] -> cast(miss), undefined
    end.

getByKey(SecKey) when is_list(SecKey) ->
    getByKey(list_to_binary(SecKey));
getByKey(SecKey) ->
    case ets:lookup(?KEY_TO_ID,SecKey) of
        [{_,Id}] -> cast(hit), Id;
        [] -> cast(miss), undefined
    end.

store(SecId, SecKey) ->
    cast({store,SecId,SecKey}).

%%%===================================================================
%%% gen_server2 callbacks
%%%===================================================================
start_link() ->
    gen_server:start_link({local,?SERVER},?MODULE,[],[]).


init([]) ->
    ?ID_TO_KEY = ets:new(?ID_TO_KEY,[named_table,protected]),
    ?KEY_TO_ID = ets:new(?KEY_TO_ID,[named_table,protected]),
    {ok,{0,0}}.

handle_call(stats,_From,State={H,M}) -> {reply, [{hit,H},{miss,M}],State};
handle_call(Request, From, State) ->
    ?lwarn("Unexpected handle_call(~p, ~p, ~p)",[Request,From,State]),
    {reply, ok, State}.

handle_cast({store,Id,Key},State) ->
    ets:insert(?KEY_TO_ID,{Key,Id}),
    case security_id:compute_key(Id) of
        Key -> ets:insert(?ID_TO_KEY,{Id,Key});
        Normalized ->
            ets:insert(?ID_TO_KEY,{Id,Normalized}),
            ets:insert(?KEY_TO_ID,{Normalized,Id})
    end,
    {noreply,State};

handle_cast(hit,{H,M}) -> {noreply,{H+1,M}};
handle_cast(miss,{H,M}) -> {noreply,{H,M+1}};
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
call(Term) -> gen_server2:call(?SERVER,Term).
