-module(mmd_cfg).
-include("mmd_cfg.hrl").
-export([new/1,update/2]).
-include_lib("p6core/include/p6core.hrl").

new(Map) -> update(#mmd_cfg{},Map).

update(Cfg,Map) ->
    updateData(2,record_info(fields,mmd_cfg),Map,Cfg).

updateData(_,[],_,Rec) -> Rec;
updateData(Idx,[Name|Names],Map,Rec) ->
    case lists:keyfind(Name,1,Map) of
        false ->
            updateData(Idx+1,Names,Map,Rec);
        {_,Val} ->
            updateData(Idx+1,Names,Map,setelement(Idx,Rec,Val))
    end.
