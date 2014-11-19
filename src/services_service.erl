-module(services_service).
-include("mmd.hrl").
-behaviour(mmd_service).

-export([service_call/2,
	 service_subscribe/2,
	 service_message/2,
	 service_close/2,
	 handle_other/2]).

service_subscribe(_Client,#channel_create{}) -> ok.
service_call(_Client,#channel_create{body=undefined}) ->
    {reply,services:allServiceNames()};
service_call(_Client,#channel_create{body=SvcPattern}) ->
    ?ldebug("Using services service"),
    Ret = lists:foldl(fun(Svc,Acc) ->
			      SB = p6str:mkbin(Svc),
			      case re:run(SB,SvcPattern) of
				  nomatch -> Acc;
				  _ -> [SB|Acc]
			      end
		      end,
		      [],
		      services:allServiceNames()),
    {reply,Ret}.

service_message(#channel_message{},_State) -> ok.
service_close(#channel_close{},_State) -> ok.
handle_other(_Other,_State) -> ok.
