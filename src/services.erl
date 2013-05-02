-module(services).
-include_lib("p6core/include/p6core.hrl").
-include("mmd.hrl").
-include_lib("p6core/include/dmap.hrl").
-compile([export_all]).

-define(SERVER, ?MODULE).
-define(P6DMAP,service_map).

-record(state, {known=sets:new(), chans=channel_mgr:new()}).

default_tags() -> p6props:getApp(mmd_core,force_tags,[]).

get_tags(Tags) ->
    Ret = case application:get_env(mmd_core,force_tags) of
	      {ok,AppTags} -> merge_tags(AppTags,Tags);
	      _ -> Tags
	  end,
    fix_tags(Ret).

fix_tags(undefined) -> undefined;
fix_tags(List) -> lists:usort(p6str:to_lower_list(List)).

merge_tags(undefined,undefined) -> undefined;
merge_tags(undefined,List) -> List;
merge_tags(List,undefined) -> List;
merge_tags(List1,List2) -> List1++List2.


normalize(S=#service{tags=undefined}) -> normalize(S#service{tags=default_tags()});
normalize(S=#service{pid=undefined}) -> normalize(S#service{pid=self(),node=node()});
normalize(S=#service{name=Name}) when is_atom(Name) -> normalize(S#service{name=p6str:to_lower_bin(Name)});
normalize(S=#service{tags=Tags}) -> S#service{tags=fix_tags(Tags)}.

regGlobal(Service=#service{pid=Pid}) ->
    S = normalize(Service),
    Name = S#service.name,
    case p6dmap:addGlobal(?P6DMAP,Pid,Name,S) of
        ok -> ?linfo("Registered global: ~p",[S]),
              ok;
        Other -> ?linfo("Failed to register global service ~p: ~p",[S,Other]),
                 Other
    end;
regGlobal(Names) when is_list(Names) -> lists:foreach(fun regGlobal/1,Names);
regGlobal(Name) -> regGlobal(#service{name=Name,pid=self(),node=node()}).

regLocal(Service=#service{pid=Pid}) ->
    S = normalize(Service),
    Name = S#service.name,
    case p6dmap:addLocal(?P6DMAP,Pid,Name,S) of
        ok -> ?linfo("Registered local: ~p (~p)",[Name,Pid]), ok;
        Other -> ?linfo("Failed to register local service ~p (~p) : ~p",[Name,Pid,Other]),
                 Other
    end;
    
regLocal(Names) when is_list(Names) -> lists:foreach(fun(Name) -> ok = regLocal(Name) end, Names);
regLocal(Name) -> regLocal(#service{name=Name,pid=self(),node=node()}).

unregGlobal(Pid, Name) ->
    case p6dmap:delGlobal(?P6DMAP, Pid, p6str:to_lower_bin(Name)) of
	ok ->
	    ?linfo("Unregistered global ~p (~p)", [Name, Pid]),
	    ok;
	Other -> ?linfo("Failed to unregister global service ~p (~p): ~p",
			[Name, Pid, Other]),
		 Other
    end.

getDM() -> whereis(?P6DMAP).

allServiceNamesUnfiltered() -> p6dmap:uniqueKeys(?P6DMAP).
allServiceNames() ->
    lists:usort(
      lists:foldl(
	fun
	    ([_Node,Key,#service{tags=Val,version=?SERVICE_VERSION},_Owner],Keys) ->
		case mmd_node_tags:has(Val) of
		    true -> [Key|Keys];
		    false -> Keys
		end;
	    (_,Keys) -> Keys
	end, 
	[], 
	p6dmap:all(?P6DMAP))).

ourServices() ->
    p6dmap:getOurEntries(?P6DMAP).

ourGlobalServices() -> p6dmap:getOurEntries(?P6DMAP,g).
ourLocalServices() -> p6dmap:getOurEntries(?P6DMAP,l).
service2Nodes() ->
    dict:to_list(lists:foldl(fun([S,N],Dict) -> dict:append(S,N,Dict) end, dict:new(), p6dmap:keyToNodes(?P6DMAP))).

%% Returns list of DM,Pid,Val
find(Name) -> filter_tags(filter_enabled(find_unfiltered(Name))).
find_unfiltered(Name) -> 
    [ S || [_P,S=#service{version=?SERVICE_VERSION}] <- p6dmap:get(?P6DMAP,p6str:to_lower_bin(Name)) ].
%%    lists:map(fun([_P,S=#service{version=?SERVICE_VERSION}]) -> S end, p6dmap:get(?P6DMAP,p6str:to_lower_bin(Name))).

findBalanced(Name) -> findBalanced(Name,undefined).
findBalanced(Name,ExcludePid) ->
    case filter_pid(ExcludePid,find(Name)) of
        [] -> [];
        List -> mmd_balance:weighted_random(List)
    end.

regProxy({T,Mod},Names) when is_list(Names) ->
    regProxy(Mod,lists:map(fun(R={_,_}) -> R;
                              (N) -> {T,N} end,
                           Names));
regProxy(Mod,Names) when is_list(Names) -> mmd_services_sup:add_service(Mod,Names);
regProxy(Mod,Name) -> regProxy(Mod,[Name]).
regProxy({Mod,Name}) -> regProxy(Mod,Name);
regProxy(Mod) -> regProxy(Mod,[Mod]).

registerConfigured() ->
    case application:get_env(services) of
        undefined -> ok;
        {ok,List} -> lists:foreach(fun(Svc)->regProxy(Svc) end, List)
    end.


init([]) ->
    mmd_node_cost:start_link(),
    mmd_node_tags:start_link(),
    cpu_load:start_link(),
    p6dmap:ensure(?P6DMAP),
    regLocal(?MODULE),
    registerConfigured(),
    timer:send_interval(500, update_svcs),
    {ok, #state{}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%% Channel Messages

handle_info(update_svcs, State) ->
    All = dispatchDiff(State),
    {noreply,State#state{known=All}};

handle_info(Info, State) ->
    ?linfo("Unexpected info: ~p, with state: ~p",[Info,State]),
    {noreply, State}.


handle_call({mmd, From, CC=#channel_create{type=call,body=?raw(<<"N">>)}}, _From, State) ->
    mmd_msg:reply(From, CC, allServiceNames()),
    {reply, ok, State};

handle_call({mmd, From, CC=#channel_create{type=call,body=undefined}}, _From, State) ->
    mmd_msg:reply(From, CC, allServiceNames()),
    {reply, ok, State};

handle_call({mmd, From, CC=#channel_create{type=call,body=SvcPattern}}, _From, State) ->
    case decode_pattern(SvcPattern) of
	{error,Reason} -> 
	    mmd_msg:error(From, CC, ?INVALID_REQUEST,
			  "Error, bad request: ~p",[Reason]);
	{ok,Str} ->
	    case re:compile(Str,[caseless]) of
		{ok, MP} ->
		    Ret = lists:foldl(fun(Svc,Acc) ->
					      SB = p6str:mkbin(Svc),
					      case re:run(SB, MP) of
						  nomatch -> Acc;
						  _ -> [SB|Acc]
					      end
				      end,
				      [],
				      allServiceNames()),
		    mmd_msg:reply(From, CC, Ret);
		{error, {ErrString, Pos}} ->
		    mmd_msg:error(From, CC, ?INVALID_REQUEST,
				  "Failed to compile regex: reason: ~p, "
				  "pos: ~p, regex: ~p", [ErrString, Pos, SvcPattern])
	    end
    end,
    {reply,ok,State};

handle_call({mmd, From, CC=#channel_create{type=sub, body=SvcPattern}}, _From,
	    State=#state{chans=Chans}) ->
    case decode_pattern(SvcPattern) of
	{error,Reason} ->
	    mmd_msg:error(From, CC, ?INVALID_REQUEST,
			  "Error, bad request: ~p",[Reason]),
	    {reply, ok, State};
	{ok,Str} ->
	    case re:compile(Str) of
		{ok, MP} ->
		    case channel_mgr:process_remote(Chans, From, CC, MP) of
			{NewChans, _Msg} ->
			    All = dispatchDiff(State),
			    Filtered =
				sets:fold(fun (Svc, Svcs) ->
						  case re:run(p6str:mkbin(Svc), MP) of
						      nomatch -> Svcs;
						      _ -> [Svc | Svcs]
						  end
					  end,
					  [],
					  All),
			    mmd_msg:reply(From, CC, ?map([{added, ?array(Filtered)}])),
			    {reply, ok, State#state{known=All, chans=NewChans}};
			NewChans ->
			    {reply, ok, State#state{chans=NewChans}}
		    end;
		{error, {ErrString, Pos}} ->
		    mmd_msg:error(From, CC, ?INVALID_REQUEST,
				  "Failed to compile regex: reason: ~p, "
				  "pos: ~p, regex: ~p", [ErrString, Pos, SvcPattern]),
		    {reply, ok, State}
	    end
    end;

handle_call({mmd, From, M=#channel_close{}}, _From,
	    State=#state{chans=Chans}) ->
    {NewChans, _Msg} = channel_mgr:process_remote(Chans, From, M),
    {reply, ok, State#state{chans=NewChans}};

handle_call(Request, From, State) ->
    ?linfo("Unexpected call: ~p from: ~p, with state: ~p",[Request,From,State]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    ?linfo("Unexpected cast: ~p, with state: ~p",[Msg,State]),
    {noreply, State}.

re_filter(MP, Es) -> re_filter(MP, Es, []).
re_filter(_MP, [], Acc) -> Acc;
re_filter(MP, [E | Es], Acc) ->
    case re:run(p6str:mkbin(E), MP) of
	nomatch -> re_filter(MP, Es, Acc);
	_ -> re_filter(MP, Es, [E | Acc])
    end.

dispatchDiff(State=#state{chans=Chans}) ->
    case channel_mgr:chan_count(Chans) of
	0 -> sets:from_list(services:allServiceNames());
	_ ->
	    {All, Add, Rm} = calcDiff(State),
	    channel_mgr:send_all_matching(
	      fun (MP) ->
		      case {re_filter(MP, Add), re_filter(MP, Rm)} of
			  {[], []} -> false;
			  {A, []} -> {true, ?map([{added, ?array(A)}])};
			  {[], R} -> {true, ?map([{removed, ?array(R)}])};
			  {A, R} -> {true, ?map([{added, ?array(A)}, {removed,?array(R)}]})
		      end
	      end, Chans),
	    All
	end.

calcDiff(#state{known=Known}) ->
    All = sets:from_list(services:allServiceNames()),
    {All,sets:to_list(sets:subtract(All,Known)),sets:to_list(sets:subtract(Known,All))}.


filter_pid(Pid,Services) ->
    lists:filter(fun(#service{pid=P}) when P == Pid -> false;
		    (_) -> true
		 end, Services).

filter_enabled(Services) -> 
    lists:filter(fun(#service{enabled=true}) -> true;
		    (_) -> false
		 end, Services).

filter_tags(Services) -> 
    lists:filter(fun(#service{node=Node}) when Node == node() -> true;
		    (#service{tags=Tags}) -> mmd_node_tags:has(Tags) 
		 end, Services).

decode_pattern(Data) ->
    case mmd_decode:decode(Data) of
	Bin when is_binary(Bin) -> {ok,Bin};
	{Bin,_} when is_binary(Bin) -> {ok,Bin};
	{undefined,_} -> {ok,<<>>};
	undefined -> {ok,<<>>};
	Other -> {error,Other}
    end.
	    

%% vim: ts=4:sts=4:sw=4:et:sta:
