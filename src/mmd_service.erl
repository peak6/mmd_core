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
-module(mmd_service).

%%% ------------------------------------------------------------------------
%%%
%%% Services running inside MMD should implement whichever methods you want to handle
%%% Unimplemented methods will log a warning and close the channel when received
%%% It's really a gen_server that honors the MMD inter-node api
%%%
%%% Note: This is not a behavior so you don't need to implement the unneccesary methods
%%%       for your service.
%%%
%%% Your module may/should export:
%%%
%%% service_call(ClientPid, #channel_create{})
%%% service_subscribe(ClientPid, #channel_create{})
%%% service_message(#channel_message{},State)
%%% service_close(#channel_close{},State)
%%% handle_info(Info,State) %% Called when mmd_service receives a raw message
%%%
%%%   Return values:
%%%     {reply,Body} | {reply,Body,_State}
%%%        This is translated to {close,Body}
%%%
%%%     {close,Body}
%%%        Replies to the client, closes the channel and stops this process
%%%
%%%     {error,Code,Reason} | {error,Code,Format,Args}
%%%        Closes the channel with an MMD Error
%%%
%%%     {noreply,State}
%%%        Don't reply now, wait for another event before replying
%%%
%%% ------------------------------------------------------------------------

-behaviour(gen_server).

-include_lib("p6core/include/p6core.hrl").
-include("mmd.hrl").

-export([behaviour_info/1]).
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state,{mod,id,mod_state,client,create}).

-define(df(M), mmd_decode:decodeFull(M)).

behaviour_info(callbacks) -> [].
%% If this were a behaivor, here are your methods
%% {service_call,2},
%% {service_subscribe,2},
%% {service_message,2},
%% {service_close,2},
%% {handle_other,2}];

% behaviour_info(_Other) -> undefined.

%%-define(DBG(Mod,Fmt,Args), ?ldebug("(~p): "++Fmt,[Mod|Args])).
-define(CREATE_CHANNEL,'$create_channel$').

start_link(Mod,From,Create) ->
    gen_server:start_link(?MODULE,[Mod,From,Create],[]).

init([Mod,From,Create=#channel_create{id=Id}]) ->
    monitor(process,From),
    self() ! ?CREATE_CHANNEL,
    {ok,#state{mod=Mod,id=Id,client=From,create=Create}}.

%% Calls
handle_info(?CREATE_CHANNEL,
	    State=#state{create=CC=#channel_create{type=call},
			 mod=Mod,
			 client=Client}) ->

    Result =  %% Map reply -> close since this is a call
	case Mod:service_call(Client,?df(CC)) of
	    {reply,Body,_} -> {close,Body};
	    {reply,Body} -> {close,Body};
	    Other -> Other
	end,
    process_result(State#state{create=undefined},Result);

%% Subscribes
handle_info(?CREATE_CHANNEL,
	    State=#state{create=CC=#channel_create{},
			 mod=Mod,
			 client=Client}) ->
    process_result(State#state{create=undefined},Mod:service_subscribe(Client,?df(CC)));

handle_info({'DOWN', _Ref, process, Client, Reason},
	    #state{client=Client, id=Id, mod=Mod, mod_state=ModState}) ->
    Mod:service_close(
      #channel_close{id=Id,
		     body=?error(?UNEXPECTED_REMOTE_CHANNEL_CLOSE,
				 p6str:mkbin(
				   "Client terminated with: ~p", [Reason]))},
      ModState),
    {stop, normal, nostate};

handle_info(Info, State=#state{mod_state=ModState,mod=Mod}) ->
    process_result(State,Mod:handle_other(Info,ModState)).

handle_call({mmd,_From,CM=#channel_message{}},Ref,State=#state{mod=Mod,mod_state=ModState}) ->
    gen_server:reply(Ref,ok),
    process_result(State,Mod:service_message(?df(CM),ModState));

handle_call({mmd,_From,CC=#channel_close{}},Ref,State=#state{mod=Mod,mod_state=ModState}) ->
    gen_server:reply(Ref,ok),
    catch Mod:service_close(?df(CC),ModState),
    {stop,normal,State};

handle_call(Msg,Ref,State) ->
    ?lwarn("Unexpected handle_call(~p, ~p, ~p)",[Msg,Ref,?DUMP_REC(state,State)]),
    {noreply,State}.

handle_cast(Msg, State) ->
    ?lwarn("Unexpected handle_cast(~p, ~p)",[Msg,?DUMP_REC(state,State)]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% For modules that have no internal state
process_result(State=#state{client=Client,id=Id},{reply,Body}) ->
    mmd_msg:reply(Client,Id,Body),
    {noreply,State};

%% Reply to client and set new module state
process_result(State=#state{client=Client,id=Id},{reply,Body,NewState}) ->
    mmd_msg:reply(Client,Id,Body),
    {noreply,State#state{mod_state=NewState}};

%% Defer responding until another event occurs
process_result(State,{noreply,NewState}) ->
    {noreply,State#state{mod_state=NewState}};

%% close channel
process_result(#state{client=Client,id=Id},{close,Body}) ->
    mmd_msg:close(Client,Id,Body),
    {stop,normal,no_state};

%% Error out channel
process_result(#state{client=Client,id=Id},{error,Code,Body}) ->
    mmd_msg:error(Client,Id,Code,Body),
    {stop,normal,no_state};

%% Error with formatting
process_result(State,{error,Code,Fmt,Args}) ->
    process_result(State,{error,Code,p6str:mkbin(Fmt,Args)});

%% Modules return this when a specific message type isn't supported
process_result(State,unsupported) ->
    process_result(State,{error,?INVALID_REQUEST,"Not supported"});

process_result(State,{raise,Type,Reason,Stack}) ->
    process_result(State,{error,?SERVICE_ERROR,"Internal service error: ~p",[Reason]}),
    erlang:raise(Type,Reason,Stack),
    ?ldebug("\n\n\nNEVER GET HERE\n\n\n");

%% Unexpected module return, error out channel
process_result(State,Other) ->
    process_result(State,{error,?SERVICE_ERROR,"Bad service return: ~p",[Other]}).

%% Captures missing methods in service.
%% This is used to send a good error message to the user when a service doesn't
%% support things like subsribe.
% do_apply(M,F,A) ->
%     try
% 	apply(M,F,A)
%     catch
% 	error:undef ->
% 	    case erlang:get_stacktrace() of
% 		[{M,F,_A,_File}|_Ignore] ->
% 		    ?lwarn("Missing service handler: ~p:~p(~p)",[M,F,A]),
% 		    {error,?INVALID_REQUEST,"Service does not support: ~p",[trans_method(F)]};
% 		Stack ->
% 		    {raise,error,undef,Stack}
% 	    end;
% 	Type:Reason ->
% 	    {raise,Type,Reason,erlang:get_stacktrace()}
%     end.


% %% Used to comp
% trans_method(service_call) -> call;
% trans_method(service_subscribe) -> subscribe;
% trans_method(service_message) -> message;
% trans_method(service_close) -> close;
% trans_method(Other) -> Other.
