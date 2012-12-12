%%%-------------------------------------------------------------------
%%% @author dbudworth@peak6.com
%%% @copyright (C) 2011, PEAK6 Investments, L.P.
%%% @doc
%%%
%%% @end
%%% Created : Wed Mar 23 09:05:59 CDT 2011
%%%-------------------------------------------------------------------
-module(socket_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-include_lib("p6core/include/logger.hrl").
-include("mmd.hrl").

-define(DEFAULT_RESTART,permanent).
-define(DEFAULT_SHUTDOWN,2000).

-define(NCHILD(Name,I, Args, Type), {Name, {I, start_link, Args}, ?DEFAULT_RESTART, ?DEFAULT_SHUTDOWN, Type, [I]}).
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

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Listeners = case application:get_env(listeners) of
                    {ok,Listener} -> process(Listener);
                    _ -> []%%?CHILD(socket_listener,worker)]
                end,
    {ok, {SupFlags, [?CHILD(socket_handlers,supervisor)|Listeners]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
process(Listeners) -> process(Listeners,[]).
process([],Acc) -> lists:reverse(Acc);
process([{mmd,Opts}|Listeners], Acc) ->
    process(Listeners,[?NCHILD(
                          make_ref(),
                          socket_listener,
                          [Opts],
                          worker)|Acc]);
%% Skip non "mmd" listeners
process([{_,_}|Listeners], Acc) -> process(Listeners,Acc).



