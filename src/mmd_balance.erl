-module(mmd_balance).
-include("mmd.hrl").

-export([weighted_random/1]).
-export([cheapest/1]).
-define(MAX_LOAD,1000).


weighted_random(Services) ->
    {Total,CS} = get_free(min_cost(Services)),
    find(random_service:uniform(Total),CS).

cheapest(Services) ->
    case get_free(min_cost(Services)) of
	{_,[]} -> undefined;
	{_,CS} -> element(2,hd(lists:sort(CS)))
    end.

find(_Num,[]) -> undefined;
find(_Num,[{_,Last}]) -> Last;
find(Num,[{Free,Svc}|_Services]) when Free >= Num -> Svc;
find(Num,[{Free,_Svc}|Services]) -> find(Num-Free,Services).

min_cost([]) -> [];
min_cost([Service=#service{node=Node}|Services]) -> min_cost(Services,mmd_node_cost:get_cost(Node),[Service]).

min_cost([],_MinCost,Acc) -> Acc;
min_cost([Service=#service{node=Node}|Services],MinCost,Acc) ->
    case mmd_node_cost:get_cost(Node) of
	N when N == MinCost -> min_cost(Services,MinCost,[Service|Acc]);
	N when N < MinCost  -> min_cost(Services,N,[Service]);
	_ -> min_cost(Services,MinCost,Acc)
    end.



get_free(Services) -> get_free(Services,0, []).
get_free([],Total,Acc) -> {Total,Acc};
get_free([S=#service{app=App,node=Node}|Services],Total,Acc) ->
    case app_free(Node,App) of %% NOTE: load must be LESS THAN MAX_LOAD to be considered for routing
	N when N >= 0 andalso N < ?MAX_LOAD -> get_free(Services,Total+N,[{N,S}|Acc]);
	_Other -> get_free(Services,Total,Acc)
    end.

app_free(Node,App) ->
    case mmd_app_load:get(Node,App) of
	undefined -> 
	    case cpu_load:util(Node) of
		undefined -> 0;
		{_,Num} when is_number(Num) -> 1000 - trunc(Num * 10)
	    end;
	N when is_number(N) -> 1000-N
    end.



    
    

