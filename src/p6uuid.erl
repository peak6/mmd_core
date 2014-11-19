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
-module(p6uuid).

-export([parse/1,safeparse/1]).
-export([next/0,next/1,nextAsString/0]).

next() -> p6uuid_mgr:next().
next(N) -> p6uuid_mgr:next(N).

nextAsString()->
    uuid:to_string(p6uuid:next()).

safeparse(Token) ->
    try parse(Token)
    catch
        _:Error -> {error,Error}
    end.

parse(Token) when is_binary(Token) -> parse(binary_to_list(Token));
parse(Token) when is_list(Token) ->
    IntList = lists:foldl(fun(N,Acc) -> {_,[X],_} = io_lib:fread("~16u",N), [X|Acc] end, [],as_byte_strings(Token,[])),
    list_to_binary(lists:reverse(IntList)).

as_byte_strings([],Acc) -> lists:reverse(Acc);
as_byte_strings([$-|Rest],Acc) -> as_byte_strings(Rest,Acc);
as_byte_strings([N1,N2|Rest],Acc) ->
    as_byte_strings(Rest,[[N1,N2]|Acc]).




