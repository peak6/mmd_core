-module(svcmgr_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-define(DEFAULT_RESTART,permanent).
-define(DEFAULT_SHUTDOWN,2000).

-define(CHILD(I, Type), {I, {I, start_link, []}, ?DEFAULT_RESTART, ?DEFAULT_SHUTDOWN, Type, [I]}).
-define(CHILD(I), ?CHILD(I,worker)).

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
    RestartStrategy = one_for_all,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 5,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Children = [
		?CHILD(cpu_load),
		?CHILD(mmd_node_cost),
		?CHILD(mmd_node_tags),
		?CHILD(services)
	       ],
    {ok, {SupFlags,Children}}.
    
    
    
