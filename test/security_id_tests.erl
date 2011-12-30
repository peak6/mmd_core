-module(security_id_tests).
-include_lib("eunit/include/eunit.hrl").

stock_test() ->
    IBM = "S:IBM",
    ?assertEqual(IBM, security_id:get_key(security_id:getID(IBM))).

put_option_test() ->
    GOOG = "O:GOOG:130619:765000:P",
    ?assertEqual(GOOG, security_id:get_key(security_id:getID(GOOG))).

call_option_test() ->
    T = "O:T:130601:765000:C",
    ?assertEqual(T, security_id:get_key(security_id:getID(T))).

future_test() ->
    NQ = "F:NQM1",
    ?assertEqual(NQ, security_id:get_key(security_id:getID(NQ))).

%% vim: ts=4:sts=4:sw=4:et:sta:
