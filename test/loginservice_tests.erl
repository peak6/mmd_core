-module(loginservice_tests).
-include_lib("eunit/include/eunit.hrl").

auth_good_test() ->
    ?assertEqual(ok,loginservice:verify_password("chicagodc.peak6.net","nextgentest","Program1")).

auth_bad_pass_test() ->
    ?assertEqual({error,invalidCredentials},loginservice:verify_password("chicagodc.peak6.net","nextgentest","Program2")).

auth_bad_server_test() ->
    ?assertEqual({error,no_auth_server},loginservice:verify_password("localhost","nextgentest","Program1")).

username_to_trader_id_test() ->
    {spawn,{setup,
		%% setup
        fun() -> 
            mongopools:dev()
        end,
        %% test
        ?_assertEqual(852,loginservice:username_to_trader_id("nextgentest"))
    }}.

bad_username_to_trader_id_test() ->
    {spawn,{setup,
		%% setup
        fun() -> 
            mongopools:dev()
        end,
        %% test
        ?_assertEqual(error,loginservice:username_to_trader_id("not_a_user"))
    }}.

can_su_test_() ->
    {spawn,{setup,
		%% setup
        fun() -> 
            mongopools:dev(),
            AuthDB = mongoapi:new(repl,<<"auth">>),
            AuthDB:save("sudoers",[{"userName", "bobptest"}])
        end,
        %% cleanup
        fun(_) -> 
            AuthDB = mongoapi:new(repl,<<"auth">>),
            AuthDB:remove("sudoers",[{"userName", "bobptest"}]) 
        end,
        %% test
        ?_assert(loginservice:can_su("bobptest"))
    }}.

can_su_fail_test_() ->
    {spawn,{setup,
        %% setup
        fun() -> 
            mongopools:dev(),
            AuthDB = mongoapi:new(repl,<<"auth">>),
            AuthDB:remove("sudoers",[{"userName", "bobptest"}]) 
        end,
        %% test
        ?_assertNot(loginservice:can_su("bobptest"))
    }}.

%% vim: ts=4:sts=4:sw=4:et:sta:
