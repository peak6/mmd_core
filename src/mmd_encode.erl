-module(mmd_encode).
-include("mmd.hrl").

-compile([export_all]).

encode_message_to_binary(Data) ->
    iolist_to_binary(encode_message(Data)).

encode(Msg) -> encode_message(Msg).

encode_message(C=#channel_create{type=call}) ->
    encode_message(C#channel_create{type=$C});
encode_message(#channel_create{service=Svc,type=Type,timeout=Timeout,auth_token=AT,id=Chan,body=Body}) ->
    [?VARINT_CHANNEL_CREATE,
     encode_uuid(Chan),
     encode_channel_type(Type),
     encode_type(string,Svc),
     encode_type(varint64,Timeout),
     encode_uuid(AT),
     encode_obj(Body)];
encode_message(#channel_message{id=Chan,body=Body}) ->
     [?CHANNEL_MESSAGE,encode_uuid(Chan),encode_obj(Body)];
encode_message(#channel_close{id=Chan,body=Body}) ->
    [?CHANNEL_CLOSE,encode_uuid(Chan),encode_obj(Body)].

encode_channel_type(sub) -> $S;
encode_channel_type(call) -> $C;
encode_channel_type(Other) -> Other.

%% Tagged types
encode_obj(?raw(Data)) -> Data;
encode_obj(Data) -> encode_obj(v1_1, Data).

encode_obj(v1_1, Obj) when is_binary(Obj) ->
    [?FAST_STRING, encode_obj(size(Obj)), Obj];
encode_obj(v1_1, 0) -> ?INT0;
encode_obj(v1_1, Obj) when is_integer(Obj) ->
    case Obj of
        O when O >= -128 andalso O < 128 -> [?INT1, <<O:8/signed>>];
        O when O > 0 andalso O < 256 -> [?UINT1, <<O:8>>];
        O when O >= -32768 andalso O < 32768 -> [?INT2, <<O:16/signed>>];
        O when O > 0 andalso O < 65536 -> [?UINT2, <<O:16>>];
        O when O >= -2147483648 andalso O < 2147483648 ->
            [?INT4, <<O:32/signed>>];
        O when O > 0 andalso O < 4294967296 -> [?UINT4, <<O:32>>];
        O when O >= -9223372036854775808 andalso
               O < 9223372036854775808 ->
            [?INT8, <<O:64/signed>>];
        O when O > 0 andalso O < 18446744073709551616 -> [?UINT8, <<O:64>>];
        O -> ?lerr("Int too big to mmd encode: ~p", [O]), O
    end;
encode_obj(_, Obj) when is_float(Obj) ->
    [?DOUBLE, encode_type(double, Obj)];
encode_obj(Vsn, Obj) when is_atom(Obj) ->
    encode_obj(Vsn, atom_to_binary(Obj, utf8));

encode_obj(_, true) -> [?TRUE];
encode_obj(_, false) -> [?FALSE];
encode_obj(_, undefined) -> [?NULL];
encode_obj(_, null) -> [?NULL];
encode_obj(_, nil) -> [?NULL];

encode_obj(Vsn, ?map(Data)) ->
    {Sz, Body} = encode_map(Vsn, Data),
    case Vsn of
        v1_1 -> [?FAST_MAP, encode_obj(Sz), Body];
        v1_0 -> [?VARINT_MAP, encode_varint(Sz), Body]
    end;
encode_obj(Vsn, ?array(Data)) ->
    {Sz, Body} = encode_array(Vsn, Data),
    case Vsn of
        v1_1 -> [?FAST_ARRAY, encode_obj(Sz), Body];
        v1_0 -> [?VARINT_ARRAY, encode_type(varint32, Sz), Body]
    end;
encode_obj(v1_1, ?error(Code, Msg)) ->
    [?VARINT_ERROR, encode_obj(Code), encode_obj(v1_1, Msg)];
encode_obj(v1_1, ?bytes(Data)) -> [?FAST_BYTES, encode_obj(size(Data)), Data];
encode_obj(_, ?byte(Byte)) ->
    [?BYTE, Byte];
encode_obj(_, ?uuid(Data)) -> [?UUID, encode_type(uuid, Data)];
encode_obj(_, ?secid(Data)) -> [?SECID, encode_type(secid, Data)];
encode_obj(v1_1,  ?time(Data)) -> [?FAST_TIME, <<Data:64>>];
encode_obj(Vsn, Obj) when is_list(Obj) -> encode_obj(Vsn, ?array(Obj));
encode_obj(Vsn, Obj) when is_tuple(Obj) ->
    encode_obj(Vsn, ?array(tuple_to_list(Obj)));

encode_obj(v1_0, Obj) when is_binary(Obj) ->
    [?VARINT_STRING, encode_varint(size(Obj)), Obj];

encode_obj(v1_0, ?error(Code, Msg)) ->
    [?VARINT_ERROR, encode_type(svarint32, Code), encode_obj(v1_0, Msg)];
encode_obj(v1_0, ?bytes(Data)) -> [?VARINT_BYTES, encode_varint(Data), Data];
encode_obj(v1_0,  ?time(Data)) -> [?VARINT_TIME, encode_svarint(Data)];
encode_obj(v1_0, Obj) when is_integer(Obj), Obj < 0 ->
    [?SVARINT64, encode_type(svarint64, Obj)];
encode_obj(v1_0, Obj) when is_integer(Obj) ->
    [?VARINT64, encode_type(varint64, Obj)].


%% Encodes a given value as the specified type
%% Note: This does NOT prefix with a type specifier, it's up to the caller
%%       to do that.
%% It is assumed that you are passing the right params here, there are no
%% validity checks
encode_type(varint64,Long) ->
    encode_varint(Long);
encode_type(svarint64,Long) ->
    encode_svarint(Long);
encode_type(varint32,Int) ->
    encode_varint(Int);
encode_type(svarint32,Int) ->
    encode_svarint(Int);
encode_type(float,Float) ->
    <<Float:32/float>>;
encode_type(double,Double) ->
    <<Double:64/float>>;
encode_type(date,D) ->
    encode_svarint(D);
encode_type(byte,B) ->
    B;
encode_type(uuid, <<UUID:16/binary>>) ->
    UUID;
encode_type(uuid, <<UUID:36/binary>>) ->
    p6uuid:parse(UUID);
encode_type(secid,SecID) when is_binary(SecID) ->
    SecID;
encode_type(string,String) when is_tuple(String) ->
    encode_type(string,p6str:mkstr("~p",String));
encode_type(string,String) when is_list(String) ->
    encode_type(string,list_to_binary(String));
encode_type(string,String) when is_atom(String) ->
    encode_type(string,atom_to_binary(String,utf8));
encode_type(string,String) when is_binary(String)->
    [encode_varint(size(String)),String].

encode_array(Vsn, Data) ->
    encode_array(Vsn, Data, 0, []).

encode_array(_Vsn, [], Sz, Acc) ->
    {Sz, lists:reverse(Acc)};
encode_array(Vsn, [Item | Items], Sz, Acc) ->
    encode_array(Vsn, Items, Sz + 1,[encode_obj(Vsn, Item) | Acc]).

encode_map(Vsn, Data) ->
    encode_map(Vsn, Data, 0, []).

encode_map(_Vsn, [], Sz, Acc) ->
    {Sz, Acc};
encode_map(Vsn, [{K, V} | Tail], Sz, Acc) ->
    encode_map(Vsn,
               Tail,
               Sz + 1,
               [encode_obj(Vsn, K), encode_obj(Vsn, V) | Acc]).

encode_uuid(Bin) when is_binary(Bin) ->
    Bin;
encode_uuid(?uuid(<<UUID:16/binary>>)) ->
    UUID;
encode_uuid(?uuid(<<UUID:36/binary>>)) ->
    p6uuid:parse(UUID).

encode_secid(Bin) when is_binary(Bin) ->
    Bin;
encode_secid(?secid(SecID)) ->
    SecID.

%% Taken from Google Protobuf integer encoding
encode_svarint(I) ->
    encode_varint(if I >= 0 -> I * 2;
                     true -> -I * 2 - 1
                  end).

encode_varint(I) when I =< 16#7f ->
    [I];
encode_varint(I) ->
    [16#80 bor (I band 16#7f) | encode_varint(I bsr 7)].
