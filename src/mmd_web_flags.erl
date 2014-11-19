-module(mmd_web_flags).
-include("mmd_cowboy_common.hrl").
-export([flags/0]).
-export([has/2]).
-export([init/3, handle/2, terminate/2]).

flags() ->
    [
     {trace,<<"Enabled verbose MMD logging for this client">>}
    ].



has(trace,Flags) -> contains(<<".trace.">>,Flags);
has(Other,Flags) -> ?lwarn("Checked for unknown flag: ~p, in cookie: ~s",[Other,Flags]),
                    false.


contains(_LookFor,<<>>) -> false;
contains(LookFor,In) -> binary:match(In,LookFor) =/= nomatch.

init({_Proto,http}, Req, Cfg) ->
    {ok,Req,Cfg}.

handle(Req,Cfg) ->
    ?ok_js([<<"var MMD_FLAGS = {">>,mk_flags(),$}],Req,Cfg).

terminate(_Req,_State) -> ok.

mk_flags() ->
    lists:map(fun({K,V})->[$",p6str:mkbin(K),$",$:,$",p6str:mkbin(V),$",$,] end,mmd_web_flags:flags()).

