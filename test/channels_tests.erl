-module(channels_tests).
-include_lib("eunit/include/eunit.hrl").

-define(assertEquiv(A,B), ?assertEqual(lists:sort(A),lists:sort(B))).
-define(DATA,[{a,1},{b,2},{c,1}]).

new_test() -> ?assertEqual([],channels:new()).

verify_data_test() ->
    D = channels:add(a,1,channels:add(b,2,channels:add(c,1,channels:new()))),
    ?assertEqual(?DATA,D).

add_new_test() -> ?assertEqual([{x,y}],channels:add(x,y,channels:new())).

refToId_test() -> ?assertEquiv([a,c],channels:refToId(1,?DATA)).

idToRef_test() -> ?assertEqual(2,channels:idToRef(b,?DATA)).

removeRef_test() -> ?assertEqual([{b,2}],channels:removeRef(1,?DATA)).

removeId_test() ->
    ?assertEquiv([{a,1},{c,1}],channels:removeId(b,?DATA)).

filterRefs_test() ->
    ?assertEqual([{b,2}], channels:filterRefs(fun(R) -> R == 2 end, ?DATA)).

partitionRef_test() ->
    ?assertEqual({[{b,2}],[{a,1},{c,1}]},channels:partitionRef(2, ?DATA)).

each_test() ->
    put(c,0),
    channels:each(fun(_,_)->put(c,get(c)+1) end,?DATA),
    ?assertEqual(3,get(c)).
