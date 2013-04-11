-module(mmd_app_load).

-include("mmd.hrl").

-behaviour(gen_server).
-export([start_link/0]).
-export([get/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-define(SERVER,?MODULE).
-define(DMAP,app_load_map).

get(Node,App) ->
    case p6dmap:getForNode(?DMAP,Node,lc(App)) of
	[] -> undefined;
	[[Val]] -> Val
    end.

start_link() ->
    gen_server:start_link({local,?SERVER},?MODULE,[],[]).

init([]) ->
    p6dmap:ensure(?DMAP),
    services:regLocal('app.load'),
    {ok,nostate}.

handle_call({mmd,From,CC=#channel_create{type=call,body=Body}},_From,State) ->
    process(From,CC,Body),
    {reply,ok,State};
handle_call({mmd,From,CC=#channel_create{}},_From,State) ->
    mmd_msg:error(From,CC,?INVALID_REQUEST,<<"Invalid Request">>),
    {reply,ok,State};
handle_call(Request, From, Load) ->
    ?lwarn("Unexpected handle_call(~p, ~p, ~p)",[Request,From,Load]),
    {reply, ok, Load}.

handle_cast(Msg, Load) ->
    ?lwarn("Unexpected handle_cast(~p, ~p)",[Msg,Load]),
    {noreply, Load}.

handle_info(Info, Load) ->
    ?lwarn("Unexpected handle_info(~p, ~p)",[Info,Load]),
    {noreply, Load}.

terminate(_Reason, _Load) ->
    ok.

code_change(_OldVsn, Load, _Extra) ->
    {ok, Load}.

process(From,CC,?raw(Raw)) -> process(From,CC,mmd_decode:decode(Raw));
process(From,CC,?map(Map)) -> 
    mmd_msg:reply(From,
		  CC,
		  do_action(p6props:getAndMap(<<"action">>, fun lc/1, Map),
			    p6props:getAndMap(<<"app">>, fun lc/1, Map),
			    p6props:get(<<"load">>,Map)));
process(From,CC,_Other) ->
    mmd_msg:error(From,CC,?INVALID_REQUEST,<<"Invalid request">>).

do_action(<<"set">>,App,Load) when Load >= 0 andalso Load =< 1000 ->
    p6dmap:setOrAddGlobal(?DMAP,App,Load);
do_action(<<"get">>,App,undefined) ->
    get(node(),App);
do_action(<<"clear">>,App,undefined) ->
    p6dmap:del(?DMAP,App);
do_action(Action,App,Load) ->
    ?error(?INVALID_REQUEST,p6str:mkbin(<<"Invalid request: ~s ( app=~s, load=~p )">>,[Action,App,Load])).

lc(Item) -> p6str:to_lower_bin(Item).





