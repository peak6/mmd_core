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
-module(mmd_decode).
-include("mmd.hrl").
-include_lib("p6core/include/logger.hrl").

-compile([export_all]).

decodeRaw(Bin) when is_binary(Bin) -> decode(?raw(Bin)).
decodeRawFull(Bin) when is_binary(Bin) -> decodeFull(?raw(Bin)).

decode(?raw(Bin)) -> decode_obj(Bin);
decode(Other) -> Other.

decodeFull(<<B/binary>>) ->
    case decode_head(B) of
        Create=#channel_create{body=Body} ->
            Create#channel_create{body=decode_obj(Body)};
        Msg=#channel_message{body=Body} ->
            Msg#channel_message{body=decode_obj(Body)};
        Close=#channel_close{body=Body} ->
            Close#channel_close{body=decode_obj(Body)};
        {Other,<<>>} -> Other;
        Other -> Other
    end;
decodeFull(Thing) -> Thing.


decode_head(<<?CHANNEL_CREATE, Chan:16/binary, Type:1/binary,
              SvcSize:8/unsigned-integer, Svc:SvcSize/binary,
              Timeout:16/signed-integer, AT:16/binary, Body/binary>>) ->
    #channel_create{service=Svc,
                    type=channel_type(Type),
                    timeout=if Timeout < 0 -> Timeout * -1; true -> Timeout*1000 end,
                    auth_token=AT,
                    id=binary:copy(Chan),
                    body=Body};
                    %body=?raw(upgrade_body(Vsn, Body))};

decode_head(<<?VARINT_CHANNEL_CREATE, Chan:16/binary, Type, Data/binary>>) ->
    {Sz, Data2} = decode_varint(Data),
    <<Svc:Sz/binary, Data3/binary>> = Data2,
    {Timeout, <<AT:16/binary, Data4/binary>>} = decode_varint(Data3),
    #channel_create{service=binary_to_atom(Svc, utf8),
                    type=channel_type(Type),
                    timeout=Timeout,
                    auth_token=AT,
                    id=binary:copy(Chan),
                    body=?raw(Data4)};

decode_head(<<?CHANNEL_MESSAGE, Chan:16/binary, Body/binary>>) ->
    #channel_message{id=Chan, body=?raw(Body)};
decode_head(<<?CHANNEL_CLOSE, Chan:16/binary, Body/binary>>) ->
    #channel_close{id=Chan, body=?raw(Body)}.

decode_obj(?TAG_WITH_SIZE(?FAST_STRING,Sz,Rest)) ->
    <<Str:Sz/binary,Rest2/binary>> = Rest,
    {Str,Rest2};
decode_obj(<<T, Data/binary>>) when T =:= ?INT0 orelse T =:= ?UINT0 ->
    {0, Data};
decode_obj(<<1:4,Sz:4,Int:Sz/unsigned-unit:8,Rest/binary>>) ->
    {Int,Rest};
decode_obj(<<0:4,Sz:4,Int:Sz/signed-unit:8,Rest/binary>>) ->
    {Int,Rest};
decode_obj(<<?DOUBLE, Double:64/float, Rest/binary>>) ->
    {Double, Rest};
decode_obj(<<?FLOAT, Float:32/float, Rest/binary>>) ->
    {Float, Rest};
decode_obj(<<?NULL,Data/binary>>) ->
    {undefined, Data};
decode_obj(<<?TRUE,Data/binary>>) ->
    {true, Data};
decode_obj(<<?FALSE,Data/binary>>) ->
    {false, Data};
decode_obj(?TAG_WITH_SIZE(?FAST_BYTES,Sz,Rest)) ->
    <<Bytes:Sz/binary,Rem/binary>> = Rest,
    {?bytes(Bytes),Rem};
decode_obj(<<?FAST_TIME,Time:64/integer,Rest/binary>>) ->
    {?time(Time),Rest};
decode_obj(<<?UUID, UUID:16/binary, Rest/binary>>) ->
    {?uuid(list_to_binary(uuid:to_string(UUID))), Rest};
decode_obj(<<?SECID, SecID:16/binary, Rest/binary>>) ->
    {?secid(SecID), Rest};
decode_obj(<<?BYTE, B, Rest/binary>>) ->
    {?byte(B),Rest};
decode_obj(?TAG_WITH_SIZE(?FAST_ARRAY,Sz,Rest)) ->
    decode_array(Rest,Sz,[]);
decode_obj(?TAG_WITH_SIZE(?FAST_MAP,Sz,Rest)) ->
    decode_map(Rest,Sz,[]);
decode_obj(<<?FAST_ERROR,Rest1/binary>>) ->
    {Code, Rest2} = decode_obj(Rest1),
    {Obj, Rest3} = decode_obj(Rest2),
    {?error(Code, Obj), Rest3};
decode_obj(M=#channel_create{}) -> M;
decode_obj(M=#channel_message{}) -> M;
decode_obj(M=#channel_close{}) -> M;
decode_obj(<<?VARINT_STRING,Data/binary>>) ->
    {Sz, PostSz} = decode_varint(Data),
    <<Str:Sz/binary, Rest/binary>> = PostSz,
    {Str,Rest};
decode_obj(<<T, Data/binary>>) when T =:= ?SVARINT64 orelse T =:= ?SVARINT32 ->
    decode_svarint(Data);
decode_obj(<<T, Data/binary>>) when T =:= ?VARINT64 orelse T =:= ?VARINT32 ->
    decode_svarint(Data);
decode_obj(<<?VARINT_TIME,Data/binary>>) ->
    {Ts, Rest} = decode_svarint(Data),
    {?time(Ts), Rest};
decode_obj(<<?VARINT_ARRAY,Bin/binary>>) ->
    {Sz,Rest} = decode_varint(Bin),
    decode_array(Rest,Sz,[]);
decode_obj(<<?VARINT_MAP,Bin/binary>>) ->
    {Sz,Data} = decode_varint(Bin),
    decode_map(Data,Sz,[]);
decode_obj(<<?VARINT_BYTES, Data/binary>>) ->
    {Sz, PostSz} = decode_varint(Data),
    <<Bytes:Sz/binary, Rest/binary>> = PostSz,
    {?bytes(Bytes), Rest};
decode_obj(<<?VARINT_ERROR, Bin/binary>>) ->
    {ErrCode, PostErr} = decode_svarint(Bin),
    {Msg, Rest} = decode_obj(PostErr),
    {?error(ErrCode, Msg), Rest};
decode_obj(<<Other:1/binary,_Rest/binary>>) ->
    throw({bad_tag,Other}).

decode_map(Bin,0,Acc) ->
    {?map(lists:reverse(Acc)),Bin};

decode_map(Bin,Sz,Acc) ->
    {K,VBin} = decode_obj(Bin),
    {V,Rem} = decode_obj(VBin),
    decode_map(Rem,Sz-1,[{K,V}|Acc]).

decode_array(Bin,0,Acc) ->
    {?array(lists:reverse(Acc)),Bin};

decode_array(Bin,Sz,Acc) ->
    {E,Rem} = decode_obj(Bin),
    decode_array(Rem,Sz-1,[E|Acc]).

decode_svarint(Bytes) ->
    {Value,Rest} = decode_varint(Bytes, 0, 0),
    {(Value bsr 1) bxor (-(Value band 1)),Rest}.

decode_varint(Bytes) ->
    decode_varint(Bytes, 0, 0).

decode_varint(<<0:1, I:7, Rest/binary>>, Acc, Shift) ->
    {(I bsl Shift) bor Acc, Rest};
decode_varint(<<1:1, I:7, Rest/binary>>, Acc, Shift) ->
    decode_varint(Rest, (I bsl Shift) bor Acc, Shift + 7).

channel_type(<<"C">>) -> call;
channel_type(<<"S">>) -> sub.

%% vim: ts=4:sts=4:sw=4:et:sta:
