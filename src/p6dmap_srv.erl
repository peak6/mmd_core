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
-module(p6dmap_srv).
-behaviour(gen_server).
-include("p6core.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-include("dmap.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% gen_server State
-record(state,{name,clientGroup}).

%% Convenience macros
-define(ENTRY(Type,Key,Val,Owner), #dm{pk={Key,Owner},owner=Owner,key=Key,val=Val,type=Type,node=node()}).
-define(REMOTE(Node,Key,Val,Owner), #dm{pk={Key,Owner},owner=Owner,key=Key,val=Val,type=g,node=Node}).
-define(DELETE(Table,Match), ets:select_delete(Table,[{Match,[],[true]}])).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name], []).

init([Name]) ->
    Name = ets:new(Name,[named_table,{keypos,2}]),
    net_kernel:monitor_nodes(true),
    castPeers(Name,gimme),
    CG = ?CG(Name),
    p6pg:start_link(CG,[{watcher,self()}]),
    {ok, #state{name=Name,clientGroup=CG}}.

handle_call(getState,_From,State) -> {reply,?DUMP_REC(state,State),State};

handle_call({add,Type,Key,Val,Owner},_From,State=#state{name=Name,clientGroup=CG}) ->
    Entry = ?ENTRY(Type,Key,Val,Owner),
    case ets:insert_new(Name,Entry) of
        true ->
			case Type of
				g -> castPeers(Name,#dmAdd{owner=Owner,key=Key,val=Val});
				_ -> ok
			end,
            p6pg:add(CG,Owner),
            {reply,ok,State};
        false -> {reply,duplicate,State}
    end;

handle_call({delete, Type, Key, Owner}, _From, State=#state{name=Name}) ->
    case ?DELETE(Name,#dm{key=Key, owner=Owner, type=Type}) of
        0 -> ?lerr("Nothing to delete for: type: ~p, key: ~p, owner: ~p",
		   [Type, Key, Owner]),
	     {reply, not_found, State};
        1 -> ?linfo("Deleted: type: ~p, key: ~p, owner: ~p",
		    [Type, Key, Owner]),
             castPeers(Name, #dmDelOwnerKey{owner=Owner, key=Key}),
	     {reply, ok, State}
    end;

handle_call(SET={set,K,V,O},_From,State=#state{name=Name}) ->
    case ets:update_element(Name,{K,O},{#dm.val,V}) of
        false -> {reply,not_found,State};
        true ->
            case getType(Name,K,O) of
                not_found -> ?lerr("Can't determine type when we just updated: ~p",[SET]),
                             {reply,not_found,State};
                g -> castPeers(Name,#dmSet{key=K,val=V,owner=O}),
                     {reply,ok,State};
                _ -> {reply,ok,State}
            end
    end;

handle_call({apply,_DM,_Fun}, _From, State) ->
    {reply, ok, State};

handle_call(Request, From, State) ->
    ?linfo("Unexpected call: ~p from: ~p, with state: ~p",[Request,From,State]),
    {reply, ok, State}.

handle_cast({Node,gimme},State=#state{name=Name}) ->
    castPeer(Node,Name,#dmState{entries=ets:match(Name,#dm{node=node(),type=g,owner='$1',key='$2',val='$3'})}),
    {noreply,State};

handle_cast({D,#dmSet{owner=O,key=K,val=V}},State=#state{name=Name}) ->
    ets:insert(Name,?REMOTE(D,K,V,O)),
    {noreply,State};

handle_cast({D,#dmAdd{owner=O,key=K,val=V}},State=#state{name=Name}) ->
    ets:insert(Name,?REMOTE(D,K,V,O)),
    {noreply,State};

handle_cast({_Node,#dmDel{key=K,owner=O}},State=#state{name=Name}) ->
    case ?DELETE(Name,#dm{key=K,owner=O}) of
        0 -> ?lwarn("Can't delete, no entry found for: ~p / ~p",[K,O]);
        1 -> ok
    end,
    {noreply,State};

handle_cast({_Node,#dmDelOwner{owner=Who}},State=#state{name=Name}) ->
    ?DELETE(Name,#dm{owner=Who}),
    {noreply,State};

handle_cast({Node,#dmDelOwnerKey{owner=Who, key=Key}},
	    State=#state{name=Name}) ->
    case ?DELETE(Name,#dm{owner=Who, key=Key}) of
        0 -> ?linfo("Nothing to delete for: owner: ~p, key: ~p", [Who, Key]);
        1 -> ?linfo("Deleted entry for: owner: ~p, key: ~p, node: ~p",
		    [Who, Key, Node])
    end,
    {noreply,State};

handle_cast({Node,#dmState{entries=E}},State=#state{name=Name}) ->
    lists:foreach(fun([P,K,V])->
                          ets:insert(Name,?REMOTE(Node,K,V,P))
                  end, E),
    ?linfo("~s synchronized ~p entries from ~s",[Name,length(E),Node]),
    {noreply,State};

handle_cast({_,{xformVals,{_,_,_}}},State) ->
    {noreply,State};

handle_cast(Msg, State) ->
    ?linfo("Unexpected cast: ~p, with state: ~p",[Msg,State]),
    {noreply, State}.


handle_info({nodeup,Node},State=#state{name=Name}) ->
    castPeer(Node,Name,gimme),
    {noreply,State};

handle_info({nodedown,Node},State=#state{name=Name}) ->
    case ?DELETE(Name,#dm{node=Node}) of
	0 -> ok;
	N -> ?linfo("Deleted ~p entries from: ~p for dead node ~p",[N,Name,Node])
    end,
    {noreply,State};

handle_info({groupMemberUp,_,_}, State) ->
    {noreply,State};

handle_info({groupMemberDown,CG,Pid}, State=#state{clientGroup=CG,name=Name}) ->
    case ?DELETE(Name,#dm{owner=Pid}) of
        0 -> ?lwarn("Received group down message but I have no entries for: ~p",[Pid]);
        _N -> castPeers(Name,#dmDelOwner{owner=Pid})
    end,
    {noreply,State};

handle_info(Info, State) ->
    ?linfo("Unexpected info: ~p, with state: ~p",[Info,State]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
castPeers(Name,Term) -> gen_server:abcast(nodes(),Name,{node(),Term}).
castPeer(Node,Name,Term) -> gen_server:cast({Name,Node},{node(),Term}).

getType(Name,Key,Owner) ->
    case ets:match(Name,#dm{pk={Key,Owner},type='$1'}) of
        [[T]] -> T;
        [] -> not_found
    end.






