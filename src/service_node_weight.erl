-module(service_node_weight).

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([getTable/0,getDC/0,getNodeWeight/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% if we are running tests, export all
-ifdef(TEST).
-compile(export_all).
-endif.

-include_lib("p6core/include/p6core.hrl").

-define(SERVER, ?MODULE).
-define(TABLE,node2weight).

-record(state,{mydc,pattern}).
%%%===================================================================
%%% API
%%%===================================================================
getTable() -> ?TABLE.
getDC() -> call(getDC).

getNodeWeight(Node) when Node == node() -> 1;

getNodeWeight(Node) when is_atom(Node) ->
    case ets:lookup(?TABLE,Node) of
        [{_,R}] -> R;
        [] -> call({getNodeWeight,Node})
    end;

getNodeWeight(Pid) when is_pid(Pid) -> getNodeWeight(node(Pid)).

start_link() ->
    gen_server:start_link({local,?SERVER},?MODULE,[],[]).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    case application:get_env(service_node_pattern) of
        {ok,Pattern} -> ok;
        _ -> Pattern = <<".*@...(...\\d+).*">>
    end,
    ets:new(?TABLE,[named_table,protected]),
    ets:insert(?TABLE,{node(),1}),
    case application:get_env(service_node_weight) of
        {ok,Weights} ->
            lists:foreach(fun(X) -> ets:insert(?TABLE,X) end, Weights);
        _ -> ok
    end,
    MyDC = getDC(node(),Pattern),
    {ok, #state{mydc=MyDC,pattern=Pattern}}.


handle_call(getDC,_From,State=#state{mydc=MyDC}) -> {reply,MyDC,State};
handle_call({getNodeWeight,Node},_From,State=#state{mydc=MyDC,pattern=Pattern}) ->
    NodeDC = getDC(Node,Pattern),
    case NodeDC of
        MyDC -> Weight = 2;
        _ -> Weight = 3
    end,
    ets:insert(?TABLE,{Node,Weight}),
    {reply,Weight,State};

handle_call(Request, From, State) ->
    ?lwarn("Unexpected handle_call(~p, ~p, ~p)",[Request,From,State]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    ?lwarn("Unexpected handle_cast(~p, ~p)",[Msg,State]),
    {noreply,State}.

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
call(Term) -> gen_server:call(?SERVER,Term).

getDC(Node,Pattern) -> re:replace(p6str:mkbin(Node),Pattern,<<"\\1">>,[{return,binary}]).
