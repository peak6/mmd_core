-module(mmd).

-compile(export_all).

start() ->
    application:start(mmd).

chanStatN(Field,Min) when is_atom(Field) ->
    lists:filter(fun({_,{_,N}}) when N >= Min -> true;
                    (_) -> false
                 end,
                 chanStat(Field)).

chanStat() -> 
    [{P,process_info(P)} || P <- clientChannels()].

chanStat(Fields) ->
    [{P,process_info(P,Fields)} || P <- clientChannels()].

clientChannelInfo() -> clientChannelInfo(all).
clientChannelInfo(Fields) ->
    [{P,client_channel:getInfo(P,Fields)} || P <- clientChannels()].

clientChannels() ->
    client_channel_sup:getChannels().



conStatN(Field,Min) when is_atom(Field) ->
    lists:filter(fun({_,{_,N}}) when N >= Min -> true;
                    (_) -> false
                 end,
                 conStat(Field)).
conStat() ->
    [{X,process_info(P)} || X={P,_} <- connections()].

conStat(Fields) ->
    [{X,process_info(P,Fields)} || X={P,_} <- connections()].
    
connections() ->
    con_tracker:getConnections().

join(Nodes) when is_list(Nodes) ->
    lists:foreach(fun join/1, Nodes);

join(Node) when Node == node() ->
    ok;
join(Node) ->
    p6auto_cluster:addNode(Node).

tempJoin(Node) ->
    net_kernel:connect(Node).

leave(Node) ->
    p6auto_cluster:delNode(Node).


%% vim: ts=4:sts=4:sw=4:et:sta:
