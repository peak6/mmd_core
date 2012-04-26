%%%-------------------------------------------------------------------
%%% @author dbudworth@peak6.com
%%% @copyright (C) 2011, PEAK6 Investments, L.P.
%%% @doc
%%%
%%% @end
%%% Created : Fri Mar 18 10:38:06 CDT 2011
%%%-------------------------------------------------------------------
-module(mmd_core_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-define(DEFAULT_RESTART,permanent).
-define(DEFAULT_SHUTDOWN,2000).

-define(CHILD(I, Args, Type), {I, {I, start_link, Args}, ?DEFAULT_RESTART, ?DEFAULT_SHUTDOWN, Type, [I]}).
-define(CHILD(I, Type), {I, {I, start_link, []}, ?DEFAULT_RESTART, ?DEFAULT_SHUTDOWN, Type, [I]}).

pubsub_child({Name, SortKey}, Type) ->
    SortKey2 = case SortKey of
		   '$node' -> node();
		   _ -> SortKey
	       end,
    {list_to_atom(Name ++ "_" ++ Type),
     {list_to_atom(Type ++ "_service"), start_link, [Name, SortKey2]},
     ?DEFAULT_RESTART,
     ?DEFAULT_SHUTDOWN,
     worker,
     []}.

mmd_tcp_client_child({Name, Host, Port}) ->
    {Name,
     {mmd_tcp_client, start_link, [Name, Host, Port]},
     ?DEFAULT_RESTART,
     ?DEFAULT_SHUTDOWN,
     worker,
     []}.

proxy_child({Name, MmdName}) ->
    {Name,
     {proxy_service, start_link, [Name, MmdName]},
     ?DEFAULT_RESTART,
     ?DEFAULT_SHUTDOWN,
     worker,
     []}.

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 5,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Http = case application:get_env(http_port) of
               undefined -> [];
               {ok,Port} -> [?CHILD(mmd_cowboy_listener,[Port],worker)]
           end,
    PubSubSpecs = case application:get_env(pub_subs) of
		      undefined ->
			  [];
		      {ok, PubSubs} ->
			  lists:foldl(fun (Spec, PS) ->
					      [pubsub_child(Spec, "pub"),
					       pubsub_child(Spec, "sub") |
					       PS]
				      end, [], PubSubs)
		  end,
    MmdTcpClientSpecs = case application:get_env(mmd_tcp_clients) of
			    undefined ->
				[];
			    {ok, Configs} ->
				lists:map(fun mmd_tcp_client_child/1, Configs)
			end,
    ProxySpecs = case application:get_env(proxies) of
		     undefined ->
			 [];
		     {ok, Cfgs} ->
			 lists:map(fun proxy_child/1, Cfgs)
		 end,
    Children = [
                ?CHILD(security_id_cache,worker),
                ?CHILD(random_service,worker),
                ?CHILD(cpu_load,worker),
                ?CHILD(client_channel_sup,supervisor),
		?CHILD(mmd_services_sup,supervisor),
                ?CHILD(mmd_monitor,worker),
                ?CHILD(service_node_weight,worker),
                ?CHILD(services,worker),
                ?CHILD(service_locations,worker),
                ?CHILD(create_tracker,worker),
                ?CHILD(con_tracker,worker),
                ?CHILD(serviceload,worker),
                ?CHILD(time_service,worker),
                ?CHILD(echo2,worker),
                ?CHILD(mmd_autodiscover,worker),
                ?CHILD(socket_sup,supervisor)
               ] ++ Http ++ PubSubSpecs ++ MmdTcpClientSpecs ++ ProxySpecs,
    {ok, {SupFlags,Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

