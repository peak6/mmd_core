-module(mmd_web_flags).
-include_lib("p6core/include/p6core.hrl").
-include("mmd_cowboy_common.hrl").
-export([flags/0]).
-export([has/2]).
-export([init/3, handle/2, terminate/2]).

flags() ->
    [
     {raw,<<"Send direct from file system.  Doesn't minify. Ignores timestamps and cache entries">>},
     {always,<<"Always send file, ignore timestamp">>},
     {trace,<<"Enabled verbose MMD logging for this client">>}
    ].


has(_,<<>>) -> false;  %% No flags set
%has(raw,Flags) -> contains(<<".raw.">>,Flags);
% always want to send raw for now. this is the simplest way
has(raw,_Flags) -> true;
has(always,Flags) -> contains(<<".always.">>,Flags);
has(trace,Flags) -> contains(<<".trace.">>,Flags);
has(Other,Flags) -> ?lwarn("Checked for unknown flag: ~p, in cookie: ~s",[Other,Flags]),
                    false.


contains(LookFor,In) -> binary:match(In,LookFor) =/= nomatch.

init({_Proto,http}, Req, Cfg) ->
    {ok,Req,Cfg}.

handle(Req,Cfg) ->
    ?ok_js([<<"var MMD_FLAGS = {">>,mk_flags(),$}],Req,Cfg).

terminate(_Req,_State) -> ok.

mk_flags() ->
    lists:map(fun({K,V})->[$",p6str:mkbin(K),$",$:,$",p6str:mkbin(V),$",$,] end,mmd_web_flags:flags()).

