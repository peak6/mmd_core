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

decode(?raw(Bin)) ->
    {Data, <<>>} = decode_obj(Bin),
    Data;
decode(Other) -> Other.

decodeFull(B) when is_binary(B) -> decodeFull(decode_head(?latest_vsn, B));
decodeFull(Create=#channel_create{body=Body}) ->
    Create#channel_create{body=decode(Body)};
decodeFull(Msg=#channel_message{body=Body}) ->
    Msg#channel_message{body=decode(Body)};
decodeFull(Close=#channel_close{body=Body}) ->
    Close#channel_close{body=decode(Body)}.

decode_head(Vsn,
            <<?CHANNEL_CREATE, Chan:16/binary, Type:1/binary,
              SvcSize:8/unsigned-integer, Svc:SvcSize/binary,
              Timeout:16/signed-integer, AT:16/binary, Body/binary>>) ->
    #channel_create{service=Svc,
                    type=channel_type(Type),
                    timeout=if Timeout < 0 -> Timeout * -1; true -> Timeout*1000 end,
                    auth_token=AT,
                    id=binary:copy(Chan),
                    body=?raw(upgrade_body(Vsn, Body))};

decode_head(Vsn,
            <<?VARINT_CHANNEL_CREATE, Chan:16/binary, Type, Data/binary>>) ->
    {Sz, Data2} = decode_varint(Data),
    <<Svc:Sz/binary, Data3/binary>> = Data2,
    {Timeout, <<AT:16/binary, Data4/binary>>} = decode_varint(Data3),
    #channel_create{service=binary_to_atom(Svc, utf8),
                    type=channel_type(Type),
                    timeout=Timeout,
                    auth_token=AT,
                    id=binary:copy(Chan),
                    body=?raw(upgrade_body(Vsn, Data4))};

decode_head(Vsn, <<?CHANNEL_MESSAGE, Chan:16/binary, Body/binary>>) ->
    #channel_message{id=Chan, body=?raw(upgrade_body(Vsn, Body))};
decode_head(Vsn, <<?CHANNEL_CLOSE, Chan:16/binary, Body/binary>>) ->
    #channel_close{id=Chan, body=?raw(upgrade_body(Vsn, Body))}.

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
    {?map(Acc),Bin};

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

downgrade_body(Vsn, Body) ->
    {NewBody, <<>>} = downgrade(Vsn, Body),
    iolist_to_binary(NewBody).

downgrade(v1_1, Body) ->
    {Body, <<>>};
downgrade(v1_0, <<?DOUBLE, Double:64/float, Rest/binary>>) ->
    {<<?DOUBLE, Double:64/float>>, Rest};
downgrade(v1_0, <<?FLOAT, Float:32/float, Rest/binary>>) ->
    {<<?FLOAT, Float:32/float>>, Rest};
downgrade(v1_0, <<?NULL, Data/binary>>) ->
    {<<?NULL>>, Data};
downgrade(v1_0, <<?TRUE, Data/binary>>) ->
    {<<?TRUE>>, Data};
downgrade(v1_0, <<?FALSE, Data/binary>>) ->
    {<<?FALSE>>, Data};
downgrade(v1_0, <<?UUID, UUID:16/binary, Rest/binary>>) ->
    {<<?UUID, UUID:16/binary>>, Rest};
downgrade(v1_0, <<?SECID, SecID:16/binary, Rest/binary>>) ->
    {<<?SECID, SecID:16/binary>>, Rest};
downgrade(v1_0, <<?BYTE, B, Rest/binary>>) ->
    {<<?BYTE, B>>, Rest};
downgrade(v1_0, ?TAG_WITH_SIZE(?FAST_STRING, Sz, Rest)) ->
    <<Str:Sz/binary, Rest2/binary>> = Rest,
    {[?VARINT_STRING, mmd_encode:encode_varint(Sz), Str], Rest2};
downgrade(v1_0, <<T, Data/binary>>) when T =:= ?INT0 orelse T =:= ?UINT0 ->
    {mmd_encode:encode_obj(v1_0, 0), Data};
downgrade(v1_0, <<1:4, Sz:4, Int:Sz/unsigned-unit:8, Rest/binary>>) ->
    {mmd_encode:encode_obj(v1_0, Int), Rest};
downgrade(v1_0, <<0:4, Sz:4, Int:Sz/signed-unit:8, Rest/binary>>) ->
    {mmd_encode:encode_obj(v1_0, Int), Rest};
downgrade(v1_0, <<?FAST_TIME, Time:64/integer, Rest/binary>>) ->
    {[?VARINT_TIME, mmd_encode:encode_svarint(Time)], Rest};
downgrade(v1_0, ?TAG_WITH_SIZE(?FAST_MAP, Sz, Rest)) ->
    {MapData, Rest2} = downgrade_map(v1_0, Rest, Sz, []),
    {[?VARINT_MAP, mmd_encode:encode_varint(Sz), MapData], Rest2};
downgrade(v1_0, ?TAG_WITH_SIZE(?FAST_ARRAY, Sz, Rest)) ->
    {ArrayData, Rest2} = downgrade_array(v1_0, Rest, Sz, []),
    {[?VARINT_ARRAY, mmd_encode:encode_varint(Sz), ArrayData], Rest2};
downgrade(v1_0, ?TAG_WITH_SIZE(?FAST_BYTES, Sz, Rest)) ->
    <<Bytes:Sz/binary, Rest2/binary>> = Rest,
    {[?VARINT_BYTES, mmd_encode:encode_varint(Sz), Bytes], Rest2};
downgrade(v1_0, <<?FAST_ERROR, Rest1/binary>>) ->
    {Code, Rest2} = decode_obj(Rest1),
    {Obj, Rest3} = decode_obj(Rest2),
    {[?VARINT_ERROR, mmd_encode:encode_svarint(Code), downgrade(v1_0, Obj)],
     Rest3}.

downgrade_array(_, Bin, 0, Acc) ->
    {iolist_to_binary(lists:reverse(Acc)), Bin};

downgrade_array(Vsn, Bin, Sz, Acc) ->
    {E, Rem} = downgrade(Vsn, Bin),
    downgrade_array(Vsn, Rem, Sz - 1, [E | Acc]).

downgrade_map(_, Bin, 0, Acc) ->
    {iolist_to_binary(Acc), Bin};

downgrade_map(Vsn, Bin, Sz, Acc) ->
    {K, VBin} = downgrade(Vsn, Bin),
    {V, Rem} = downgrade(Vsn, VBin),
    downgrade_map(Vsn, Rem, Sz - 1, [K, V | Acc]).

upgrade_body(Vsn, Body) ->
    {NewBody, <<>>} = upgrade(Vsn, Body),
    iolist_to_binary(NewBody).

upgrade(v1_1, Body) ->
    {Body, <<>>};
upgrade(v1_0, <<?DOUBLE, Double:64/float, Rest/binary>>) ->
    {<<?DOUBLE, Double:64/float>>, Rest};
upgrade(v1_0, <<?FLOAT, Float:32/float, Rest/binary>>) ->
    {<<?FLOAT, Float:32/float>>, Rest};
upgrade(v1_0, <<?NULL, Data/binary>>) ->
    {<<?NULL>>, Data};
upgrade(v1_0, <<?TRUE, Data/binary>>) ->
    {<<?TRUE>>, Data};
upgrade(v1_0, <<?FALSE, Data/binary>>) ->
    {<<?FALSE>>, Data};
upgrade(v1_0, <<?UUID, UUID:16/binary, Rest/binary>>) ->
    {<<?UUID, UUID:16/binary>>, Rest};
upgrade(v1_0, <<?SECID, SecID:16/binary, Rest/binary>>) ->
    {<<?SECID, SecID:16/binary>>, Rest};
upgrade(v1_0, <<?BYTE, B, Rest/binary>>) ->
    {<<?BYTE, B>>, Rest};
upgrade(v1_0, <<?VARINT_STRING, Data/binary>>) ->
    {Sz, PostSz} = decode_varint(Data),
    <<Str:Sz/binary, Rest/binary>> = PostSz,
    {[?FAST_STRING, mmd_encode:encode_obj(Sz), Str], Rest};
upgrade(v1_0, <<T, Data/binary>>) when T == ?SVARINT64 orelse T == ?SVARINT32->
    {N, Rest} = decode_svarint(Data),
    {mmd_encode:encode_obj(N), Rest};
upgrade(v1_0, <<T, Data/binary>>) when T == ?VARINT64 orelse T == ?VARINT32->
    {N, Rest} = decode_varint(Data),
    {mmd_encode:encode_obj(N), Rest};
upgrade(v1_0, <<?VARINT_TIME,Data/binary>>) ->
    {Ts, Rest} = decode_svarint(Data),
    {<<?FAST_TIME, Ts:64/integer>>, Rest};
upgrade(v1_0, <<?VARINT_MAP, Bin/binary>>) ->
    {Sz, Data} = decode_varint(Bin),
    {MapData, Rest} = upgrade_map(v1_0, Data, Sz, []),
    {[?FAST_MAP, mmd_encode:encode_obj(Sz), MapData], Rest};
upgrade(v1_0, <<?VARINT_ARRAY, Bin/binary>>) ->
    {Sz, Rest} = decode_varint(Bin),
    {ArrayData, Rest2} = upgrade_array(v1_0, Rest, Sz, []),
    {[?FAST_ARRAY, mmd_encode:encode_obj(Sz), ArrayData], Rest2};
upgrade(v1_0, <<?VARINT_BYTES, Data/binary>>) ->
    {Sz, PostSz} = decode_varint(Data),
    <<Bytes:Sz/binary, Rest/binary>> = PostSz,
    {[?FAST_BYTES, mmd_encode:encode_obj(Sz), Bytes], Rest};
upgrade(v1_0, <<?VARINT_ERROR, Bin/binary>>) ->
    {ErrCode, PostErr} = decode_svarint(Bin),
    {Body, Rest} = upgrade(v1_0, PostErr),
    {[?FAST_ERROR, mmd_encode:encode_obj(ErrCode), Body], Rest}.

upgrade_array(_, Bin, 0, Acc) ->
    {iolist_to_binary(lists:reverse(Acc)), Bin};

upgrade_array(Vsn, Bin, Sz, Acc) ->
    {E, Rem} = upgrade(Vsn, Bin),
    upgrade_array(Vsn, Rem, Sz - 1, [E | Acc]).

upgrade_map(_, Bin, 0, Acc) ->
    {iolist_to_binary(Acc), Bin};

upgrade_map(Vsn, Bin, Sz, Acc) ->
    {K, VBin} = upgrade(Vsn, Bin),
    {V, Rem} = upgrade(Vsn, VBin),
    upgrade_map(Vsn, Rem, Sz - 1, [K, V | Acc]).

%% vim: ts=4:sts=4:sw=4:et:sta:
