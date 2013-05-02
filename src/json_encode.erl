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
-module(json_encode).

-export([encode/1]).
-include("mmd.hrl").
-include_lib("p6core/include/logger.hrl").

encode(?raw(Bin)) -> encode(mmd_decode:decodeRawFull(Bin));

encode(Data) ->
    Obj = encode_obj(Data),
    {ok,json:encode(Obj)}.



uuidToStr(?uuid(UUID)) -> uuidToStr(UUID);
uuidToStr(?secid(SECID)) -> uuidToStr(SECID);
uuidToStr(Bin) -> list_to_binary(uuid:to_string(Bin)).


encode_obj(undefined) -> null;
encode_obj(nil) -> null;
encode_obj(null) -> null;
encode_obj(true) -> true;
encode_obj(false) -> false;
encode_obj(A) when is_atom(A) -> p6str:mkbin(A);
encode_obj(?error(C,M)) when is_integer(C) -> {obj,[{'_mmd_error',C},{msg,encode_obj(M)}]};
encode_obj(?time(Time)) -> {obj,[{'_mmd_time',Time}]};
encode_obj(#channel_create{id=Id,body=Body,auth_token=AT,service=Svc,type=T}) ->
    {obj,[{T,uuidToStr(Id)},{service,Svc},{token,uuidToStr(AT)},{body,encode_obj(Body)}]};
encode_obj(#channel_message{id=Id,body=Body}) -> {obj,[{msg,uuidToStr(Id)},{body,encode_obj(Body)}]};
encode_obj(#channel_close{id=Id,body=Body}) ->{obj,[{close,uuidToStr(Id)},{body,encode_obj(Body)}]};
encode_obj(?uuid(<<Id:36/binary>>)) -> {obj,[{'_mmd_uuid',Id}]};
encode_obj(?uuid(<<Id:16/binary>>)) -> {obj,[{'_mmd_uuid',uuidToStr(Id)}]};
%%encode_obj(?secid(Id)) -> {obj,[{'_mmd_secid',uuidToStr(Id)}]};
encode_obj(?map(undefined)) -> {obj,[]};
encode_obj(?map(Map)) -> {obj,lists:map(fun({K,V}) -> {p6str:mkbin(K),encode_obj(V)} end,Map)};
encode_obj(?array(undefined)) -> [];
encode_obj(?array(Arr)) -> lists:map(fun(V) -> encode_obj(V) end, Arr);
encode_obj(?raw(Data)) -> encode_obj(mmd_decode:decodeRawFull(Data));
encode_obj(?secid(Id)) -> security_id:get_key(Id);
encode_obj(?bytes(Bin)) -> encode_obj(mmd_decode:decodeFull(Bin));
encode_obj(Pid) when is_pid(Pid) -> p6str:mkbin(Pid);
encode_obj(T) when is_tuple(T) -> encode_obj(tuple_to_list(T));
encode_obj(List) when is_list(List) -> lists:map(fun(X) -> encode_obj(X) end, List);
encode_obj(Val) -> Val.

