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
%%% @author rbeaty@peak6.com
%%% @copyright (C) 2011, PEAK6 Investments, L.P.
%%% @doc
%%%
%%% @end
%%% Created : Mon Mar 28 12:05:18 CDT 2011
%%%-------------------------------------------------------------------
-module(serviceregistry).

-include("mmd.hrl").
-behavior(mmd_service).

-export([service_call/2]).
%% originator=O is the ORIGIN of the call (ie: the socket)
service_call(_Client,#channel_create{originator=O, body=?map(Props)}) ->
    {reply,do_action(O,Props)};

service_call(_Client,#channel_create{originator=O,body=Body}) when is_binary(Body) ->
    {reply,do_action(O,[{<<"name">>,Body},{<<"action">>,<<"register">>}])}.

do_action(Originator, Props) ->
    [Names,Action,Tags] = p6props:anyAndMap([
					     {<<"name">>,fun to_lower_list/1},
					     {<<"action">>,fun p6str:mkatom/1},
					     {<<"tag">>,fun to_lower_list/1}
					    ],Props),
    case validate(Names) of
	[] -> do_action(Action,Originator,Names,Tags);
	Other -> ?error(?INVALID_REQUEST,p6str:mkbin(Other))
    end.

to_lower_list(Items) when is_list(Items) -> lists:map(fun p6str:to_lower_bin/1,Items);
to_lower_list(Item) -> to_lower_list([Item]).

validate(undefined) -> {error,name_not_specified};
validate(Names) ->
    lists:foldl(fun
		    (<<>>,Acc) -> [{error,empty_name}|Acc]; 
		    (_,Acc) -> Acc
		end, [], Names).

do_action(register, O, Names,Tags ) ->
    Results = lists:foldl(fun(Name,Acc) ->
				  case services:regGlobal(O,Name,Tags) of
				      ok -> Acc;
				      Other -> [{Name,Other}|Acc]
				  end
			  end,
			  [],
			  Names),
    case Results of
	[] -> ok;
	Other -> ?error(?INVALID_REQUEST,Other)
    end;

do_action(unregister, O, Names ,_Tags) ->
    lists:foreach(fun (Name) ->
			  services:unregGlobal(O, p6str:to_lower_bin(Name))
		  end, Names);

do_action(_Action, _O, _Names,_Tags) ->
    ?ldebug("Action: ~p, Originator: ~p, Names: ~p, Tags: ~p",[_Action,_O,_Names,_Tags]),
    ?error(?INVALID_REQUEST, <<"Invalid action or invalid args for action">>).


%% vim: ts=4:sts=4:sw=4:et:sta:
