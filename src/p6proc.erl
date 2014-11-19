-module(p6proc).

-compile([export_all]).


info(Procs,Stat) when is_list(Procs) ->
    [{P,info(P,Stat)} || P <- Procs];

info(Name,Stat) when is_atom(Name) ->
    case whereis(Name) of
        Pid when is_pid(Pid) -> info(Pid,Stat);
        _Other -> undefined
    end;

info(Pid,Stats) when is_list(Stats) ->
    [ info(Pid,Stat) || Stat <- Stats ];

info(Pid,{Stat,FromUnit,ToUnit}) ->
    case info(Pid,Stat) of
        undefined -> undefined;
        {_,undefined} -> undefined;
        {_,N} -> {Stat,p6mem:convert(N,FromUnit,ToUnit)}
    end;

info(Pid,Stat) when is_pid(Pid) andalso is_atom(Stat) ->
    process_info(Pid,Stat).

