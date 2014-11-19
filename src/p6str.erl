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
-module(p6str).

-export([mkio/1,mkio/2]).
-export([mkbin/1,mkbin/2]).
-export([mkstr/1,mkstr/2]).
-export([mkatom/1,mkatom/2,mkexatom/2]).
-export([concat/1]).
-export([timeToStr/1]).
-export([ip_to_str/1,ip_port_to_str/2,ip_port_to_bin/2]).
-export([sock_to_str/1,local_sock_to_str/1]).
-export([sock_to_ipstr/1,local_sock_to_ipstr/1]).
-export([sock_to_port/1,local_sock_to_port/1]).
-export([full_sock_bin/2,full_remote_sock_bin/1,full_local_sock_bin/1]).
-export([to_lower/1,to_upper/1]).
-export([to_lower_bin/1,to_upper_bin/1]).
-export([to_lower_list/1]).
-export([to_integer/1,to_float/1]).
-export([ends_with/2]).
-export([join/2]).

-define(format, utf8).

ends_with(Subject,EndsWith) when is_binary(Subject), is_binary(EndsWith) ->
    size(EndsWith) == binary:longest_common_suffix([Subject,EndsWith]);
ends_with(Subject,EndsWith) when is_list(Subject), is_list(EndsWith) ->
    lists:suffix(EndsWith,Subject).

join(List,With) when is_binary(With) ->
    binary:list_to_bin(join(List,With,[]));
join(List,With) when is_list(With) ->
    string:join(List,With).

join([A],_,Acc) -> lists:reverse([A|Acc]);
join([L|Rest],With,Acc) -> join(Rest,With,[With,L|Acc]).


to_float(Bin) when is_binary(Bin) -> to_float(binary_to_list(Bin));
to_float(List) when is_list(List) ->
    case catch list_to_float(List) of
        {'EXIT',{badarg,_}} ->
            float(list_to_integer(List));
        Float -> Float
    end;
to_float(Float) when is_float(Float) -> Float;
to_float(Int) when is_integer(Int) -> float(Int).

to_integer(Bin) when is_binary(Bin) -> list_to_integer(binary_to_list(Bin));
to_integer(List) when is_list(List) -> list_to_integer(List);
to_integer(Int) when is_integer(Int) -> Int.

%% A = 65, Z = 90
to_lower(I) when I > 64 andalso I < 91 -> I + 32;
to_lower(L) when is_list(L) -> [ to_lower(C) || C <- L ];
to_lower(Bin) when is_binary(Bin) -> << <<(to_lower(B))>> || <<B:8>> <= Bin >>;
to_lower(Other) -> Other.


to_lower_bin(Str) -> << <<(to_lower(A))>> || <<A>> <= mkbin(Str) >>.

to_lower_list(Items) when is_list(Items) ->
    [ to_lower_bin(I) || I <- Items ];
to_lower_list(Item) -> [to_lower_bin(Item)].

%% a = 97, z = 122
to_upper(I) when I > 96 andalso I < 123 -> I - 32;
to_upper(L) when is_list(L) -> [ to_upper(C) || C <- L ];
to_upper(Bin) when is_binary(Bin) -> << <<(to_upper(B))>> || <<B:8>> <= Bin >>;
to_upper(Other) -> Other.

to_upper_bin(Str) -> << <<(to_upper(A))>> || <<A>> <= mkbin(Str) >>.

ip_to_str({A,B,C,D}) ->
    mkstr("~p.~p.~p.~p",[A,B,C,D]).

ip_port_to_str({A,B,C,D},Port) ->
    mkstr("~p.~p.~p.~p:~p",[A,B,C,D,Port]).

ip_port_to_bin({A,B,C,D},Port) ->
    mkbin("~p.~p.~p.~p:~p",[A,B,C,D,Port]).

local_sock_to_str(Sock) ->
    {ok,{H,P}} = inet:sockname(Sock),
    ip_port_to_str(H,P).

local_sock_to_ipstr(Sock) ->
    {ok,{{A,B,C,D},_Port}} = inet:sockname(Sock),
    mkstr("~p.~p.~p.~p",[A,B,C,D]).

sock_to_port(Sock) ->
    {ok,{_,Port}} = inet:peername(Sock),
    Port.

local_sock_to_port(Sock) -> 
    {ok,{_,Port}} = inet:sockname(Sock),
    Port.

sock_to_ipstr(Sock) ->
    {ok,{{A,B,C,D},_Port}} = inet:peername(Sock),
    mkstr("~p.~p.~p.~p",[A,B,C,D]).

sock_to_str(Sock) ->
    {ok,{H,P}} = inet:peername(Sock),
    ip_port_to_str(H,P).

full_local_sock_bin(Sock) -> full_sock_bin(Sock,<<"->">>).
full_remote_sock_bin(Sock) -> full_sock_bin(Sock,<<"<-">>).
full_sock_bin(Sock,Dir) ->
    {ok,{{LA,LB,LC,LD},LP}} = inet:sockname(Sock),
    {ok,{{RA,RB,RC,RD},RP}} = inet:peername(Sock),
    mkbin("~p.~p.~p.~p:~p~s~p.~p.~p.~p:~p",
          [LA,LB,LC,LD,LP,Dir,RA,RB,RC,RD,RP]).
    
mkio(Bin) when is_binary(Bin) -> [Bin];
mkio(Atom) when is_atom(Atom) -> atom_to_list(Atom);
mkio(Int) when is_integer(Int) -> integer_to_list(Int);
mkio(Float) when is_float(Float) -> float_to_list(Float);
mkio(Str=[I|_]) when is_integer(I) -> Str;
mkio(Other) -> io_lib:format("~p",[Other]).

mkio(Fmt,Args) -> io_lib:format(Fmt,Args).

mkbin(Bin) when is_binary(Bin) -> Bin;
mkbin(Atom) when is_atom(Atom) -> atom_to_binary(Atom,?format);
mkbin(Int) when is_integer(Int) -> list_to_binary(integer_to_list(Int));
mkbin(Float) when is_float(Float) -> list_to_binary(float_to_list(Float));
mkbin(List) when is_list(List) -> try list_to_binary(List) catch error:badarg -> mkbin("~p",[List]) end;
mkbin(Other) -> mkbin("~p",[Other]).

mkbin(Fmt,Args) -> iolist_to_binary(io_lib:format(Fmt,Args)).


mkstr([]) -> [];
mkstr(Bin) when is_binary(Bin) -> binary_to_list(Bin);
mkstr(Atom) when is_atom(Atom) -> atom_to_list(Atom);
mkstr(Str=[I|_]) when is_integer(I) -> Str;
mkstr(Int) when is_integer(Int) -> integer_to_list(Int);
mkstr(Float) when is_float(Float) -> float_to_list(Float);
mkstr(Other) -> mkstr("~p",[Other]).

mkstr(Fmt,Args) -> lists:flatten(io_lib:format(Fmt,Args)).

concat(List) -> concat(List,[]).

concat([],Acc) ->
    lists:flatten(lists:reverse(Acc));
concat([Part|Parts],Acc) when is_binary(Part) ->
    concat(Parts,[binary_to_list(Part)|Acc]);
concat([Part|Parts],Acc) when is_list(Part) ->
    concat(Parts,[Part|Acc]).

mkatom(Atom) when is_atom(Atom) -> Atom;
mkatom(Bin) when is_binary(Bin) -> erlang:binary_to_atom(Bin,?format);
mkatom(String) -> list_to_atom(String).

mkexatom(Fmt,Args) ->
    list_to_existing_atom(mkstr(Fmt,Args)).

mkatom(Fmt,Args) ->
    list_to_atom(mkstr(Fmt,Args)).

timeToStr(NowTime={_,_,Micro}) ->
    DateTime=calendar:now_to_local_time(NowTime),
    {{Year,Month,Day},{Hour,Min,Sec}} = DateTime,
    iolist_to_binary(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B.~B",
                                   [Year, Month, Day, Hour, Min, Sec,Micro])).



