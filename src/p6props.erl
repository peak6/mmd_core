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
-module(p6props).

-export([first/2,first/3]).
-export([has/2,hasAny/2]).
-export([get/2,get/3,getAndMap/3]).
-export([getApp/1,getApp/2,getApp/3]).
-export([any/2,any/3]).
-export([all/2]).
-export([anyAndMap/2,anyAndMap/3]).
-export([put_all/2]).

put_all(Src,Target) ->
    lists:foldl(fun({K,_}=Entry,Result) ->
			lists:keystore(K,1,Result,Entry)
		end,
		Target,
		Src).

has(Key,PList) ->
    lists:any(fun({K,_}) -> K == Key end,PList).

hasAny(Keys,PList) ->
    lists:any(fun(K) -> has(K,PList) end,Keys).

getApp(Key) ->
    application:get_env(Key).

getApp(Key,Default) ->
    case application:get_env(Key) of
        undefined -> Default;
        Other -> Other
    end.

getApp(App, Key, Default) ->
    case application:get_env(App,Key) of
        {ok,Val} -> Val;
	_ -> Default
    end.

get(Key,PList,Default) ->
    case proplists:lookup(Key,PList) of
        none -> Default;
        {_,Val} -> Val
    end.

getAndMap(Key,Fun,PList) ->
    case get(Key,PList) of
	undefined -> undefined;
	Val -> Fun(Val)
    end.

get(Key,PList) ->
    get(Key,PList,undefined).

first([],_,Default) -> Default;
first([P|Rest],PList,Default) ->
    case proplists:lookup(P,PList) of
        none -> first(Rest,PList,Default);
        Other -> Other
    end.

first(Keys,PList) ->
    first(Keys,PList,undefined).

anyAndMap(KeyMap,PList) -> anyAndMap(KeyMap,PList,undefined).
anyAndMap(KeyMap,PList,Default) ->
    {Keys,Maps} = lists:unzip(KeyMap),
    First = any(Keys,PList,'$kmdefault'),
    lists:map(fun({'$kmdefault',{_,Val}}) -> Val;
                 ({'$kmdefault',_}) -> Default;
                 ({V,{F,_}}) -> F(V);
                 ({V,F}) -> F(V)
              end,
              lists:zip(First,Maps)).

any(Keys,PList) -> any(Keys,PList,undefined).
any(Keys,PList,Default) -> any(Keys,PList,Default,[]).

any([],_PList,_Default,Acc) -> lists:reverse(Acc);
any([Key|Rest],PList,Default,Acc) -> any(Rest,PList,Default,[get(Key,PList,Default)|Acc]).

all(Keys,PList) -> all(Keys,PList,[]).

all([],_PList,Result) -> {ok,lists:reverse(Result)};
all([Key|Keys],PList,Result) ->
    case lists:keyfind(Key,1,PList) of
        false -> {error,{missing,Key}};
        {Key,Val} -> all(Keys,PList,[Val|Result])
    end.


