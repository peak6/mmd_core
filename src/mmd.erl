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

connections() ->
    lists:map(fun(C) -> {_,A} = ?DUMP_REC(con_info,C), A end, con_tracker:getConnections()).

join(Nodes) when is_list(Nodes) ->
    lists:foreach(fun join/1, Nodes);

join(Node) when Node == node() ->
    ok;
join(Node) -> net_kernel:connect(Node).

get_dc() -> get_dc(node()).
get_dc(Node) -> re:replace(p6str:to_lower_bin(Node),<<"^(.*@)?...(....).*$">>,<<"\\2">>,[{return,binary}]).

%% vim: ts=4:sts=4:sw=4:et:sta:
