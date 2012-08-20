%%%-------------------------------------------------------------------
%%% @author Max New mnew@peak6.com
%%% @copyright (C) 2012, PEAK6 Investments, L.P.
%%% @doc
%%%
%%% @end
%%% Created : 20 Aug 2012
%%%-------------------------------------------------------------------
-module(script_launcher_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(SCRIPTS_DIR, "priv/scripts").
-define(DEFAULT_SHUTDOWN,2000).

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
    case supervisor:start_link({local, ?SERVER}, ?MODULE, []) of
	{ok, Pid} ->
	    Files = script_files(),
	    lists:foreach(fun start_child/1, Files),
	    {ok, Pid};
	Other -> Other
    end.

%%--------------------------------------------------------------------
%% @doc
%%   Launches a child that launches a script at Filename
%% @spec
%% @end
%%--------------------------------------------------------------------
start_child(Filename) ->
    supervisor:start_child(?SERVER, [Filename]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    ChildSpec = {script_launcher, {script_launcher, start_link, []},
		 transient, 2000, worker, [script_launcher]},

    %% AChild = {'AName', {'AModule', start_link, []},
    %% 	      Restart, Shutdown, Type, ['AModule']},

    {ok, {SupFlags, [ChildSpec]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
script_files() ->
    case filelib:is_dir(?SCRIPTS_DIR) of
	true ->
	    {ok, Contents} = file:list_dir(?SCRIPTS_DIR),
	    FullPaths = lists:map(fun(F) -> filename:join(?SCRIPTS_DIR, F) end,
		      Contents),
	    lists:filter(fun filelib:is_regular/1, FullPaths);
	_ ->
	    []
    end.
			      
    
