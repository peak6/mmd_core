%% Copyright 2011 PEAK6 Investments, L.P.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
-module(p6dmap).
-compile([export_all]).

-include("p6core.hrl").
-include("dmap.hrl").

ensure(Name) -> 
    case lists:member(Name,allMaps()) of
	true -> ok;
	false -> new(Name)
    end.

new(Name) -> p6dmap_sup:create_dmap(Name).

allState(Name) -> [{P,gen_server:call(P,getState)} || P <- [whereis(Name)|peers(Name)] ].
allMaps() -> [N || {N,_,_,_} <- supervisor:which_children(p6dmap_sup)].
dump(Name) -> ets:tab2list(Name).
dumpAll() -> [ {N,dump(N)} || N <- allMaps() ].

keySet(Name) -> keySet(Name,'_').

make_any_guard([Item]) -> {'==','$1',Item};
make_any_guard([Item|Items]) -> {'orelse',{'==','$1',Item},make_any_guard(Items)}.

%% Find elements with the specified key, returns [Key,Owner,Value]
any(Name,Keys) -> ets:select(Name,[{#dm{key='$1',owner='$2',val='$3'},[make_any_guard(Keys)],['$$']}]).


getWithDM(Name,Key) -> ets:match(Name,#dm{key=Key,node='$1',owner='$2',val='$3'}).
get(Name,Key) -> ets:match(Name,#dm{key=Key,owner='$1',val='$2'}).
get(Name,Owner,Key) -> ets:match(Name,#dm{key=Key,val='$1',owner=Owner}).

all(Name) -> ets:match(Name,#dm{node='$1',key='$2',val='$3',owner='$4'}).

uniqueKeys(Name) -> lists:usort([K || [K] <- ets:match(Name,#dm{key='$1'})]).

del(Name,Type,Owner,Key) -> call(Name,{delete,Type,Key,Owner}).
add(Name,Type,Owner,Key,Val) -> call(Name,{add,Type,Key,Val,Owner}).
set(Name,Owner,Key,Val) -> call(Name,{set,Key,Val,Owner}).

setOrAddGlobal(Name,Key,Val) ->
    case set(Name,Key,Val) of
	not_found -> addGlobal(Name,Key,Val);
	Other -> Other
    end.

keySet(Name,Type) -> sets:from_list( [ K || [K] <- ets:match(Name,#dm{key='$1',type=Type})]).

state(Name) -> call(Name,getState).
peers(Name) -> p6pg:members(?PG(Name)).

count(Name,Owner,Key) ->
    ets:select_count(Name,[{#dm{key=Key,owner=Owner},[],[true]}]).

keyToNodes(Name) -> ets:match(Name,#dm{node='$2',key='$1'}).

getOwnerEntries(Name,Owner) -> getOwnerEntries(Name,Owner,'_').
getOwnerEntries(Name,Owner,Type) -> ets:match(Name,#dm{owner=Owner,key='$1',val='$2',type=Type}).
getNodeEntries(Name,Type,Node) -> ets:match(Name,#dm{owner='$1',key='$2',val='$3',type=Type,node=Node}).
getForNode(Name,Node,Key) -> ets:match(Name,#dm{node=Node,key=Key,val='$1'}).
getOurEntries(Name) -> getNodeEntries(Name,'_',node()).
getOurEntries(Name,Type) -> getNodeEntries(Name,Type,node()).

addLocal(Name,Key,Val) -> addLocal(Name,self(),Key,Val).
addLocal(Name,Owner,Key,Val) -> add(Name,l,Owner,Key,Val).
addGlobal(Name,Key,Val) -> addGlobal(Name,self(),Key,Val).
addGlobal(Name,Owner,Key,Val) -> add(Name,g,Owner,Key,Val).

delGlobal(Name, Owner, Key) -> del(Name, g, Owner, Key).

%% Takes a fun (Pid,Key,Val) -> MutatedVal
transformValues(Name,Fun) -> call(Name,{xform,Fun}).

set(Name,Key,Val) -> set(Name,self(),Key,Val).

del(Name, Key) -> del(Name, g, self(), Key).

keyCount(Name,Key) -> count(Name,'_',Key).

call(Name,Term) -> gen_server:call(Name,Term).
cast(Name,Term) -> gen_server:cast(Name,Term).
castAll(Name,Term) -> gen_server:abcast(Name,{node(),Term}).
callAll(Name,Term) -> gen_server:multi_call(Name,{node(),Term}).


