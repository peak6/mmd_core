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


service_call(_Client,#channel_create{originator=O, %% This is the ORIGIN of the call (ie: the socket)
				     body=?map([{ActionBin, Value}])}) ->
    Action = p6str:mkatom(ActionBin),
    {reply,do_action(Action,O,Value)};

service_call(_Client,#channel_create{originator=O,body=Body}) when is_binary(Body) ->
    case validateName(Body) of
        ok -> {reply,services:regGlobal(O,p6str:mkatom(Body),0)};
        Other -> {reply,?error(?INVALID_REQUEST,p6str:mkbin(Other))}
    end.

do_action(register, O, ?array(Names)) ->
    case lists:foldl(fun (Name, Acc) ->
			     case validateName(Name) of
				 ok -> Acc;
				 O -> [p6str:mkbin({Name, O}) | Acc]
			   end
		   end, [], Names) of
	[] ->
	    lists:foreach(fun (Name) ->
				  services:regGlobal(O, p6str:mkatom(Name), 0)
			  end, Names),
	    ok;
	Errors ->
	    ?error(?INVALID_REQUEST, ?array(Errors))
    end;

do_action(register, O, Name) ->
    case validateName(Name) of
        ok -> services:regGlobal(O, p6str:mkatom(Name), 0);
        Other -> ?error(?INVALID_REQUEST,p6str:mkbin(Other))
    end;

do_action(registerUnique, O, {'$array', [Name, Key]}) ->
    Key2 = case Key of
	       <<"$node">> -> node();
	       _ -> Key
	   end,

    case validateName(Name) of
        ok -> services:regUnique(O, p6str:mkatom(Name), [unique, Key2]);
        Other -> ?error(?INVALID_REQUEST,p6str:mkbin(Other))
    end;

do_action(unregister, O, ?array(Names)) ->
    lists:foreach(fun (Name) ->
			  services:unregGlobal(O, p6str:mkatom(Name))
		  end, Names);
do_action(unregister, O, Name) ->
    services:unregGlobal(O, p6str:mkatom(Name));
do_action(_Action, _O, _Name) ->
    ?error(?INVALID_REQUEST, <<"Invalid action or invalid args for action">>).


validateName(<<>>) -> {error,empty_name};
validateName([]) -> {error,empty_name};
validateName('') ->{error,empty_name};
validateName(_) -> ok.
%% vim: ts=4:sts=4:sw=4:et:sta:
