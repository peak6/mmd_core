-module(p6init_file).

-compile([export_all]).

-include("p6init.hrl").

loadFile(Env=#env{file=OFile},File) ->
    (loadFile(Env#env{file=File}))#env{file=OFile}.

loadFile(Env=#env{file=File}) ->
    case file:path_consult(Env#env.paths,File) of
        {ok,Items,LoadedFile} ->
            ?ldebug(Env,"Loaded: ~p from: ~p",[LoadedFile,Env#env.paths]),
            NewEnv = Env#env{paths=[filename:dirname(File)|Env#env.paths]},
            lists:foldl(
              fun ({include,F},E) -> loadFile(E,F);
                  (Other,E) -> E#env{entries=[Other|E#env.entries]}
              end,
              addLoaded(LoadedFile,NewEnv),
              lists:reverse(Items));
        Err -> ?die(Env,"Failed to load: ~p, ~p",[File,Err])
    end.

find(_Fun,[]) -> undefined;
find(Fun,[I|Items]) ->
    case Fun(I) of
        true -> I;
        _ -> find(Fun,Items)
    end.

addLoaded(File,Env=#env{paths=Paths,loaded=Loaded}) ->
    AbsFile = filename:absname(File),
    Abs = filename:dirname(AbsFile),
    ?linfo(Env,"Loaded: ~p",[AbsFile]),
    case lists:member(Abs,Paths) of
        true -> Env#env{loaded=[AbsFile|Loaded]};
        false -> Env#env{paths=Paths++[Abs],loaded=[AbsFile|Loaded]}
    end.

