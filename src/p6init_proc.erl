-module(p6init_proc).
-include("p6init.hrl").
-compile([export_all]).

proc(Env) ->
    proc(Env,Env#env.entries).

proc(Env=#env{curItem=Cur},Item) when Cur /= Item -> proc(Env#env{curItem=Item},Item);
proc(Env,Items) when is_list(Items) -> lists:foldl(fun(I,E)->proc(E,I) end, Env, Items);
proc(Env,{'$cookie',Cookie}) -> setCookie(Env,Cookie);
proc(Env,'$debug') -> Env#env{log=?DEBUG};
%%proc(Env,'$start') -> startApp(Env);
proc(Env,{start,Apps}) -> lists:foldl(fun(A,E)->startApp(E,A) end,Env,Apps);
proc(Env,{include,File}) -> ?loadFile(Env,File);
proc(Env,{apply,{M,F,A}}) -> doApply(Env,M,F,A);
proc(Env,{set,A,Items}) -> procSet(Env,A,Items);
proc(Env,{add,A,Items}) -> procAdd(Env,A,Items);
proc(Env,{env,E,Items}) -> procEnv(Env,E,Items);
proc(Env,{initLager,Props}) -> p6init_lager:initLager(Env,Props);
%proc(Env,{K,V}) -> setItem(Env,K,V);
%proc(Env,Ignored={_,_,_}) -> ?ldebug(Env,"Ignoring: ~p~n"
%                                     "     Env:~n~s",[Ignored,Env]);
proc(Env,Ignored) -> ?die(Env,"Don't know what to do with: ~p",[Ignored]).

procEnv(Env,E,Items) when is_atom(E) -> procEnv(Env,[E],Items);
procEnv(Env=#env{env=E},Envs,Items) ->
    case lists:member(E,Envs) of
        true -> proc(Env,Items);
        false -> Env
    end.

procAdd(Env,App,Items) ->
    tryLoad(Env,App),
    lists:foreach(fun({K,V}) when is_list(V) ->
						  DCVals = [ getDCKey(Elem) || Elem <- V ],
                          case application:get_env(App,K) of
                              undefined -> setEnv(Env,App,K,DCVals);
                              {ok,L} when is_list(L) -> setEnv(Env,App,K,L++DCVals);
                              {ok,T} -> setEnv(Env,App,K,[T,DCVals])
                          end
                  end,Items),
    Env#env{app=App}.

procSet(Env,App,Items) ->
    tryLoad(Env,App),
    lists:foreach(fun({K,V}) -> setEnv(Env,App,K,getDCKey(V)) end, Items),
    Env#env{app=App}.

getDCKey(V) when is_list(V) ->
	case proplists:get_value(getDatacenter(), V) of
		undefined -> V;
		Else -> Else
	end;
getDCKey(V) -> V.

getDatacenter() -> {ok, Host} = inet:gethostname(), getDatacenter(Host).
getDatacenter(Host) when is_list(Host) -> getDatacenter(list_to_binary(Host));
getDatacenter(<<_BizChar:1/binary, "slchi6", _Rest/binary>>) -> chi6;
getDatacenter(<<_BizChar:1/binary, "slchi5", _Rest/binary>>) -> chi5;
getDatacenter(_Other) -> chi6.

doApply(Env,M,F,A) ->
    ?ldebug(Env,"Applying: ~p:~p(~p)",[M,F,A]),
    erlang:apply(M,F,A),
    Env.


setCookie(Env=#env{env=E},{Node,'$env'}) ->
    setCookie(Env,{Node,p6str:mkatom("mmd_~s",[E])});
setCookie(Env,{Node,Cookie}) ->
    ?ldebug("Setting cookie: ~p / ~p",[Node,Cookie]),
    true = erlang:set_cookie(Node,Cookie),
    Env;
setCookie(Env,Cookie) -> setCookie(Env,{node(),Cookie}).

setEnv(E,A,K,V) ->
    ?ldebug(E,"Setting: ~p/~p = ~p",[A,K,V]),
    application:set_env(A,K,V),
    E.

tryLoad(Env,App) ->
    case appStatus(Env,App) of
        undefined ->
            case application:load(App) of
                ok ->
                    case application:get_key(App,applications) of
                        {ok,List} -> lists:foreach(fun(A)->tryLoad(Env,A) end, List);
                        undefined -> ?die(Env,"Dont think ok is ok here"),ok
                    end,
                    Env;
                {error,{already_loaded,_}} -> Env;
                Other -> ?die(Env,"Error loading application '~p': ~p",[App,Other])
            end;
        _ -> ok
    end.

startApp(Env=#env{app=App}) -> startApp(Env,App).
startApp(Env,App) ->
    case appStatus(Env,App) of
        started -> Env;
        undefined -> tryLoad(Env,App), doStart(Env,App);
        loaded -> doStart(Env,App);
        Ignore -> ?linfo(Env,"Ignoring: ~p / ~p ",[App,Ignore]),
                  Env
    end.

doStart(Env,App) ->
    case App of
        undefined -> ?die(Env,"No application specified, please use -p6app APPNAME");
        _ -> ok
    end,
    case application:get_key(App,applications) of
        {ok,List} ->
            ?linfo(Env,"Starting application '~p', depends on: ~p",[App,List]),
            lists:foreach(fun(A)->startApp(Env,A) end, List);
        undefined ->
            ?linfo(Env,"Starting application '~p'",[App]),
            ok
    end,
    ?ldebug(Env,"~p vars before starting: ~p",[App,application:get_all_env(App)]),
    case application:start(App) of
        ok ->
            ?ldebug(Env,"~p vars after starting: ~p",[App,application:get_all_env(App)]),
            Env;
        {error,{already_started,App}} -> ?ldebug(Env,"Already started '~p'",[App]);
        Other -> ?die(Env,"Error starting: ~p, ~p",[App,Other])
    end.

appStatus(_Env,App) ->
    Info = application:info(),
    States = [State || {State,_} <- lists:filter(fun({_,Items}) -> lists:keymember(App,1,Items)  end, Info)],

    case lists:reverse(States) of
        [] -> undefined;
        [loading|_] -> loaded;
        [starting|_] -> started;
        [running|_] -> started;
        [Other|_] -> Other
    end.

