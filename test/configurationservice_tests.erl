-module(configurationservice_tests).
-include_lib("eunit/include/eunit.hrl").
-include("mmd.hrl").

get_collection_global_test() ->
    AT = #auth_token{userName = "nextgentest",loginTime = os:timestamp(), 
                    lastAccess = os:timestamp(), userId = 852, token = uuid:random()},
    Body = [{<<"protection">>,<<"global">>},{<<"application">>,<<"appName">>}],
    ?assertEqual({<<"cfg_global">>,"app_appName"}, configurationservice:get_collection(AT, Body)).

get_collection_firm_test() ->
    {spawn,{setup,
		%% setup
        fun() -> 
            mongopools:dev()
        end,
        %% test
        fun() ->
            AT = #auth_token{userName = "nextgentest",loginTime = os:timestamp(), 
                            lastAccess = os:timestamp(),userId = 852,token = uuid:random()},
            Body = [{<<"protection">>,<<"firm">>}, {<<"firm">>, <<"PEAK6">>}],
            ?_assertEqual({<<"cfg_firm">>,"frm_1"},configurationservice:get_collection(AT,Body))
        end
    }}.

get_collection_user_test() ->
    {spawn,{setup,
		%% setup
        fun() -> 
            mongopools:dev()
        end,
        %% test
        fun() ->
            AT = #auth_token{userName = "nextgentest",loginTime = os:timestamp(), 
                            lastAccess = os:timestamp(),userId = 852,token = uuid:random()},
            Body = [{<<"protection">>,<<"user">>}],
            ?_assertEqual({<<"cfg_user">>,"usr_852"},configurationservice:get_collection(AT,Body))
        end
    }}.

%% vim: ts=4:sts=4:sw=4:et:sta:
