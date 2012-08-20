%%%-------------------------------------------------------------------
%%% @author Max New mnew@peak6.com
%%% @copyright (C) 2012, PEAK6 Investments, L.P.
%%% @doc
%%%
%%% @end
%%% Created : 20 Aug 2012
%%%-------------------------------------------------------------------
-module(script_launcher).
-include_lib("p6core/include/logger.hrl").
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {file, port}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts a server that spawns an os process using executable Filename
%%
%% @spec start_link(Filename) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Filename) ->
    gen_server:start_link(?MODULE, [Filename], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([File]) ->
    Port = start_executable(File, []),
    {ok, #state{file=File,
	       port=Port}}.


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({Port, {exit_status, Status}}, #state{port=Port}=State) ->
    case Status of
	0 ->
	    {stop, normal, State};
	N ->
	    {stop, {exit_status, N}, State}
    end;
handle_info({Port, {data, Data}}, #state{port=Port} = State) ->
    ?linfo(Data),
    {noreply, State}.




%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(normal, _State) ->
    ok;
terminate({exit_status, _N}, _State) ->
    ok;
terminate(_Reason, #state{port=Port}) ->
    erlang:port_close(Port).



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
start_executable(File, Args) ->
    erlang:open_port({spawn_executable, File}, [exit_status, {args, Args}]).
