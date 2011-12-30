%%%-------------------------------------------------------------------
%%% @author dbudworth@peak6.com
%%% @copyright (C) 2011, PEAK6 Investments, L.P.
%%% @doc
%%%
%%% @end
%%% Created : Sun Apr 24 10:56:38 CDT 2011
%%%-------------------------------------------------------------------
-module(client_channel_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([new/3,new/4]).
-export([getChannels/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-define(DEFAULT_RESTART,temporary).
-define(DEFAULT_SHUTDOWN,2000).

-define(CHILD(I, Args, Type), {I, {I, start_link, Args}, ?DEFAULT_RESTART, ?DEFAULT_SHUTDOWN, Type, [I]}).
-define(CHILD(I, Type), {I, {I, start_link, []}, ?DEFAULT_RESTART, ?DEFAULT_SHUTDOWN, Type, [I]}).
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

new(OwnerPid,ChanCreate,Cfg) -> supervisor:start_child(?SERVER,[OwnerPid,ChanCreate,Cfg]).
new(OwnerPid,ChanCreate,SvcPid,Cfg) -> supervisor:start_child(?SERVER,[OwnerPid,ChanCreate,SvcPid,Cfg]).

getChannels() -> lists:map(fun({_,P,_,_}) -> P end, supervisor:which_children(?SERVER)).
%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 1,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Children = [
		?CHILD(client_channel,worker)
	       ],
    {ok, {SupFlags, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

