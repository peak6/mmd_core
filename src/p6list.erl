-module(p6list).

-export([contains_all/2]).

contains_all([],_) -> true;
contains_all([H|T],CheckIn) ->
    case lists:member(H,CheckIn) of
	true -> contains_all(T,CheckIn);
	false -> false
    end.

    
