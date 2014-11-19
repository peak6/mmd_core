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

-module(p6file).
-export([read_lines/2]).
-export([pathRead/2]).
-export([pathFindType/3]).
-export([readLink/1]).
-export([fileType/1]).
-export([lastMod/1,lastModGMT/1]).
-export([resolveDots/1]).
-export([absPath/1]).
-export([real_path/1,real_path/2]).
-export([join/1,join/2]).

-include_lib("kernel/include/file.hrl").
-include("p6core.hrl").

join(List = [Name|_Rest]) when is_binary(Name) ->
    re:replace(p6str:join(List,<<"/">>),<<"/+">>,<<"/">>,[{return,binary},global]);
join(List = [[N|_]|_Rest]) when is_integer(N) ->
    re:replace(p6str:join(List,<<"/">>),<<"/+">>,<<"/">>,[{return,list},global]).

join(Name1,Name2) when is_binary(Name2) -> join([Name1,Name2]);
join(Name,Name2=[N|_]) when is_integer(N) -> join([Name,Name2]);
join(Name,Names) when is_list(Names) -> join([Name|Names]).

absPath(Path = [$/|_Ignore]) -> resolveDots(Path);
absPath(Path) -> resolveDots(p6str:mkstr("~s/~s",[okget:ok(file:get_cwd()),Path])).

%% Returns Path with all links resolved
real_path(Path) ->
    [Root|Rest] = filename:split(filename:absname(Path)),
    real_path(Root,Rest).

real_path(Base,Path) when is_binary(Path) ->
    case filename:split(Path) of
        [<<$/>>|Rest] -> real_path(Base,Rest);
        Other -> real_path(Base,Other)
    end;

%% Returns Base + Resolved path elements
real_path(Path,[]) -> {ok,resolveDots(Path)};
real_path(Base,[P|Ath]) ->
    FP = filename:join(Base,P),
    case file:read_link(FP) of
	{error,einval} ->
	    real_path(FP,Ath);
	{ok,RP=[$/|_]} -> real_path(RP,Ath);
	{ok,P} -> {error,eloop};
	{ok,L} ->
	    real_path(Base,filename:split(L)++Ath);
	Other -> Other
    end.

resolveDots(Path) -> resolveDots(filename:split(Path),[]).

resolveDots([],N) -> filename:join(lists:reverse(N));
resolveDots(["."|ORest],N) -> resolveDots(ORest,N);
resolveDots([".."|ORest],[_Remove|NRest]) -> resolveDots(ORest,NRest);
resolveDots([<<".">>|ORest],N) -> resolveDots(ORest,N);
resolveDots([<<"..">>|ORest],[_Remove|NRest]) -> resolveDots(ORest,NRest);
resolveDots([O|ORest],N) -> resolveDots(ORest,[O|N]).

readLink(File) ->
    case file:read_link(File) of
        %% einval generated when the referenced file is there, but not a link
        %% so just return the original path
        {error,einval} -> {ok,File};
        Other -> Other
    end.

lastModGMT(File) ->
    case lastMod(File) of
	E = {error,_} -> E;
	TS ->  calendar:local_time_to_universal_time_dst(TS)
    end.

lastMod(File) ->
    case filelib:last_modified(File) of
	0 -> {error,enoent};
	Other -> {ok,Other}
    end.

fileType(File) ->
    case file:read_file_info(File) of
        E={error,_} -> E;
        {ok,#file_info{type=regular}} -> file;
        {ok,#file_info{type=directory}} -> dir
    end.

pathFindType([],_Name,_Type) -> {error,enoent};
pathFindType([Dir|Rest],Name,Type) ->
    Check = Dir ++ "/" ++ Name,
    case fileType(Check) of
        T when T == Type -> {T,Check};
        _ -> pathFindType(Rest,Name,Type)
    end.

pathRead(Path,File) ->
    case pathFindType(Path,File,file) of
        {file,F} -> file:read_file(F);
        Other -> Other
    end.

read_lines(File,N) ->
    case file:open(File,[read]) of
        {ok,IO} ->
            readAndClose(IO,N);
        Other -> Other
    end.

finish(IODev,Acc) -> file:close(IODev),
                     lists:reverse(Acc).

readAndClose(IODev,N) -> readAndClose(IODev,N,[]).
readAndClose(IODev,0,Acc) -> finish(IODev,Acc);
readAndClose(IODev,N,Acc) ->
    case file:read_line(IODev) of
        {ok,Data} -> readAndClose(IODev,N-1,[Data|Acc]);
        eof -> finish(IODev,Acc);
        Other -> Other
    end.



