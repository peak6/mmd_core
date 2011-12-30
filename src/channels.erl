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
-module(channels).
-include_lib("p6core/include/logger.hrl").

-export([new/0]).
-export([each/2]).
-export([add/3]).
-export([hasId/2]).
-export([idToRef/2,refToId/2]).
-export([removeRef/2,removeId/2]).

-export([filterRefs/2,partitionRef/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Keeps a N-1 mapping of Channel ID -> Reference
%% This is specificaly generic as to what a "Ref" is so it can be used by connectors and services.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new() -> [].

add(Id,Ref,Chans) -> [{fixid(Id),Ref}|Chans].

hasId(Id,Chans) ->
    case lists:keyfind(Id,1,Chans) of
        false -> false;
        _ -> true
    end.
                     
%% Returns a list of Id's matching the given ref
refToId(Ref,Chans)  ->
    [ Id || {Id,CRef} <- Chans, CRef == Ref ].

%% Returns the single ref associated with this Id
idToRef(Id,Chans) ->
    case lists:keyfind(fixid(Id),1,Chans) of
        false -> not_found;
        {_,Ref} -> Ref
    end.

removeId(Id,Chans) ->
    lists:keydelete(Id,1,Chans).

removeRef(Ref,Chans) ->
    lists:filter(fun({_,R}) -> R /= Ref end, Chans).

partitionRef(Ref,Chans) ->
    lists:partition(fun({_,R}) -> R == Ref end,Chans).

filterRefs(Fun,Chans) ->
    lists:filter(fun({_,Ref}) -> Fun(Ref) end, Chans).

%% Expects a fun/2 that takes Ref,Id as args, return val is discarded
each(Fun,Chans) ->
    lists:foreach(fun({Id,Ref}) -> Fun(Id,Ref) end, Chans).

fixid(Id) -> Id.
