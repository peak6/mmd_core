%% ets record

-record(dm,{pk='_',owner='_',key='_',val='_',type='_',node='_'}).
-define(PG(Name), p6str:mkatom("~s_peers",[Name])).
-define(CG(Name), p6str:mkatom("~s_clients",[Name])).

-record(dmAdd,{key,val,owner}).
-record(dmState,{entries}).
-record(dmSet,{key,val,owner}).
-record(dmDel,{key,owner}).
-record(dmDelOwner,{owner}).
-record(dmDelOwnerKey,{owner, key}).
