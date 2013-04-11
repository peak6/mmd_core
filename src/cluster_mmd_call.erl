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
%%%-------------------------------------------------------------------
%%% @author dbudworth@peak6.com
%%% @copyright (C) 2011, PEAK6 Investments, L.P.
%%% @doc
%%%
%%% @end
%%% Created : Thu Mar 31 07:12:25 CDT 2011
%%%-------------------------------------------------------------------
-module(cluster_mmd_call).

-include_lib("p6core/include/p6core.hrl").
-include("mmd.hrl").
-export([call/4]).

-define(DOWN(Ref,Pid,Reason), {'DOWN',Ref,process,Pid,Reason}).

%%%===================================================================
%%% API
%%%===================================================================
-record(call,{svc,body,at=?NO_AUTH,timeout=10000}).

call(Service,Body,AuthToken,0) ->
    ?linfo("Changed infinite timeout to 10000ms for: ~p",[Service]),
    call(Service,Body,AuthToken,10000);
call(Service,Body,AuthToken,Timeout) ->
    call(#call{svc=Service,body=Body,at=AuthToken,timeout=Timeout}).

call(Call = #call{svc=Svc}) ->
    case services:find(Svc) of
        [] -> {error,not_found};
        Svcs -> call( [Pid || #service{pid=Pid} <- Svcs], Call)
    end.

call(Pids, Call = #call{}) ->
    Me = self(),
    Pid = proc_lib:spawn(fun() -> doCall(Me,Pids,Call) end),
    Ref = erlang:monitor(process,Pid),
    receive
        {ok,Response} -> erlang:demonitor(Ref,[flush]),
                         Response;
        Other ->
            ?lwarn("Unexpected: ~p",[Other])
    end.

doCall(Parent,Pids,Call=#call{timeout=Timeout}) ->
    erlang:send_after(Timeout, self(), timeout),
    WaitFor = [ fire(Pid,Call) || Pid <- Pids ],
    Parent ! waitFor(WaitFor, []).

fire(Pid,#call{svc=Svc,timeout=Timeout,body=Body,at=AT}) ->
    CC = #channel_create{service=Svc,timeout=Timeout,auth_token=AT,body=Body,type=call},
    {ok,CCPid} = client_channel:new(self(),CC,Pid),
    Ref = erlang:monitor(process,CCPid),
    {Ref,CC#channel_create.id,CCPid,Pid}.


waitFor([], Rec) -> {ok, Rec};
waitFor(WaitFor, Received) ->
    receive
        {'$gen_call',GSRef,{mmd,_From,#channel_close{id=Id,body=Body}}} ->
            gen_server:reply(GSRef,ok),
            case lists:keyfind(Id,2,WaitFor) of
                {Ref,Id,_Pid,SvcPid} ->
                    erlang:demonitor(Ref),
                    waitFor(lists:keydelete(Id, 2, WaitFor),
			    [{SvcPid, Body} | Received])
            end;
        ?DOWN(Ref,Pid,Reason) ->
            case lists:keyfind(Ref,1,WaitFor) of
                {Ref,_Id,_CCPid,_Pid} ->
		    waitFor(lists:keydelete(Ref,1,WaitFor),
			    [{Pid, ?error(?SERVICE_ERROR,
					  p6str:mkbin("Service error: ~p",
						      [Reason]))} | Received]);
                false ->
		    ?lwarn("Dunno what ~p/~p/~p is",[Ref,Pid,Reason]),
		    waitFor(WaitFor, Received)
            end;
	timeout ->
	    timeout(WaitFor, Received);
        Other ->
            ?lwarn("Unexpected: ~p",[Other]),
            waitFor(WaitFor, Received)
    end.

timeout(WaitFor,Received) ->
    ?linfo("Signaling timeouts for: ~p",[WaitFor]),
    {ok,lists:map(fun({_Ref,Id,CCPid,Pid}) ->
                          Err = ?error(?TIMEOUT,timeout),
                          mmd_msg:dispatch(CCPid,#channel_close{id=Id,body=Err}),
                          {Pid,Err}
                  end, WaitFor)++Received}.
