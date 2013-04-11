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
    case p6props:anyAndMap([{<<"action">>, fun p6str:mkatom/1},
			    {<<"name">>,fun fix_names/1}
			   ],Props) of
	[undefined,_] -> ?error(?INVALID_REQUEST,<<"Action must be specified">>);
	[_,undefined] -> ?error(?INVALID_REQUEST,<<"Name (String or List of Strings) must be specified">>);
	[_,Err=?error(_,_)] -> Err;
	[Action,Names] -> do_action(Action,Originator,Names,Props)
    end.

false_or_true(false) -> false;
false_or_true(<<"false">>) -> false;
false_or_true(_) -> true.

fix_names(?array(Arr)) -> fix_names(Arr);
fix_names(undefined) -> undefined;
fix_names([]) -> undefined;
fix_names(Bin) when is_binary(Bin) -> fix_names([Bin],[]);
fix_names(List) when is_list(List) -> fix_names(List,[]).

fix_names([], Acc) -> Acc;
fix_names([Name|Names], Acc) ->
    case fix_name(Name) of
	<<>> -> ?error(?INVALID_REQUEST,<<"Name cannot be an empty string">>);
	FixedName when is_binary(FixedName) -> fix_names(Names,[FixedName|Acc])
    end.
		
fix_name(Name) when is_binary(Name) -> p6str:to_lower_bin(Name).
		  
do_action(register, Originator, Names, Props ) ->
    Results = lists:foldl(fun(Name,Acc) ->
				  case services:regGlobal(mkreg(Originator,Name,Props)) of
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

do_action(unregister, Originator, Names ,_Props) ->
    lists:foreach(fun (Name) ->
			  services:unregGlobal(Originator, p6str:to_lower_bin(Name))
		  end, Names);

do_action(Action, _Originator, _Names, Props) ->
    ?ldebug("Invalid action: ~p, props: ~p",[Action,Props]),
    ?error(?INVALID_REQUEST, <<"Invalid action or invalid args for action">>).

mkreg(Originator,Name,Props) ->
    #service{pid=Originator,
	     name=Name,
	     node=node(Originator),
	     app=p6str:to_lower_bin(p6props:get(<<"app">>,Props,<<"default">>)),
	     enabled=false_or_true(p6props:get(<<"enabled">>,Props)),
	     tags=fix_tags(p6props:get(<<"tag">>,Props))
	    }.

fix_tags(undefined) -> undefined;
fix_tags(?array(Tags)) -> fix_tags(Tags);
fix_tags(Tag) when is_binary(Tag) -> [p6str:to_lower_bin(Tag)];
fix_tags(Tags) when is_list(Tags) -> p6str:to_lower_list(Tags).

%% vim: ts=4:sts=4:sw=4:et:sta:
