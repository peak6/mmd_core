-module(p6init_args).
-include("p6init.hrl").
-compile([export_all]).

process_args(StartEnv,Args) ->
    Env = lists:foldl(
            fun
                ({p6noexit,[]},E) -> E#env{noexit=true};
                ({p6init,["debug"]},E) -> E#env{log=?DEBUG};
                ({p6init,["info"]},E) -> E#env{log=?INFO};
                ({p6init,["all"]},E) -> E#env{log=?ALL};
                ({p6app,[App]},E) -> E#env{app=list_to_atom(App)};
                ({p6cfg,[File]},E) -> ?loadFile(E#env{file=File});
                (Other,E) -> ?ldebug(E,"Ignoring argument: ~p",[Other])
            end,
            StartEnv,
            Args),
    case Env#env.loaded of
        [] -> case p6init_file:find(fun filelib:is_regular/1,["p6init.cfg","priv/p6init.cfg"]) of
                  undefined -> ?die(Env,"No -p6cfg on command line, and can't find default p6init.cfg");
                  Other -> p6init_proc:proc(?loadFile(Env,Other))
              end;
        _ -> p6init_proc:proc(Env)
    end.
