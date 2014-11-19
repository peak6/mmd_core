-module(p6init_log).
-include("p6init.hrl").
-compile([export_all]).

ldebug(E,M) -> ldebug(E,M,[]).
linfo(E,M) -> linfo(E,M,[]).
ldebug(E,M,A) -> log(E,?DEBUG,M,A).
linfo(E,M,A) -> log(E,?INFO,M,A).
log(E=#env{log=L},Level,M,A) when L >= Level -> io:format(""++M++"~n",mapArgs(A)),E;
log(E,_,_,_) -> E.

mapArgs(Args) ->
    lists:map(fun(E=#env{}) -> envToTable(E);
                 (O) -> O
              end,
              Args).

die(Env,Msg) ->
    die(Env,Msg,[]).
die(Env,Msg,Args) ->
    io:format("~n      Error : "++Msg++"~n",Args),
%                "Environment~n~s",Args++[envToTable(Env)]),
    case Env#env.noexit of
        true -> Env;
        _ -> timer:sleep(50), halt(1)
    end.

envToTable(Env=#env{}) ->
    lists:map(fun({K,V}) -> io_lib:format("~11s : ~p~n",[K,V]) end,
              lists:zip(record_info(fields,env),tl(tuple_to_list(Env)))).
