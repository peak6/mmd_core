-module(mmd).

-compile(export_all).
-include("con_tracker.hrl").
-include("mmd.hrl").

start() ->
    application:start(mmd).

add_tag(Tag) -> mmd_node_tags:add(Tag).
get_tags() -> mmd_node_tags:get().
del_tag(Tag) -> mmd_node_tags:remove(Tag).


channels() ->
    client_channel_sup:getChannels().

nodes_per_service(Svc) ->
    lists:map(fun(N)->{N,rpc:call(N,mmd,channels_for,[Svc])} end, [node()|nodes()]).

channels_for(Svc) when is_atom(Svc) -> channels_for(p6str:mkbin(Svc));
channels_for(Svc) ->
    lists:filter(fun(P) -> 
			 {state,X} = client_channel:getInfo(P), 
			 re:run(proplists:get_value(svc,X),Svc) =/= nomatch 
		 end, 
		 channels()).

connections() ->
    lists:map(fun(C) -> {_,A} = ?DUMP_REC(con_info,C), A end, con_tracker:getConnections()).

join(Nodes) when is_list(Nodes) ->
    lists:foreach(fun join/1, Nodes);

join(Node) when Node == node() ->
    ok;
join(Node) -> net_kernel:connect(Node).

get_dc() -> get_dc(node()).
get_dc(Node) -> re:replace(p6str:to_lower_bin(Node),<<"^(.*@)?...(....).*$">>,<<"\\2">>,[{return,binary}]).

version()->
    application:load(mmd_core), % load app in case being called from script
    case application:get_key(mmd_core,vsn) of
	{ok, Version}->Version;
	_ -> undefined
    end.
	
		 

	


%% vim: ts=4:sts=4:sw=4:et:sta:
