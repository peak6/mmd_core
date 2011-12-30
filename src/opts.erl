-module(opts).

-compile([export_all]).

get(Name,Default,List) ->
    case lists:keyfind(Name,1,List) of
        false -> Default;
        {Name,Val} -> Val
    end.
