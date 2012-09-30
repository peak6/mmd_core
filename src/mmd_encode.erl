-module(mmd_encode).
-include("mmd.hrl").

-compile([export_all]).

encode_message_to_binary(Data) ->
    iolist_to_binary(encode_message(Data)).

encode(Vsn, Msg) -> encode_message(Vsn, Msg).

encode_message(M) ->
    encode_message(?latest_vsn, M).

encode_message(v1_1, #channel_create{service=Svc, type=Type, timeout=Timeout,
                                     auth_token=AT, id=Chan, body=Body}) ->
    TypeBin = encode_channel_type(Type),
    SvcSz = iolist_size(Svc),
    BodyBin = encode_obj(v1_1, Body),
    [<<?CHANNEL_CREATE,
       Chan:16/binary,
       TypeBin,
       SvcSz:8/unsigned-integer,
       Svc:SvcSz/binary,
       Timeout:16/signed-integer,
       AT:16/binary>>,
     BodyBin];
encode_message(v1_0, #channel_create{service=Svc, type=Type, timeout=Timeout,
                                     auth_token=AT, id=Chan, body=Body}) ->
    [?VARINT_CHANNEL_CREATE,
     encode_uuid(Chan),
     encode_channel_type(Type),
     encode_varint(iolist_size(Svc)),
     Svc,
     encode_varint(Timeout),
     encode_uuid(AT),
     encode_obj(v1_0, Body)];
encode_message(Vsn, #channel_message{id=Chan, body=Body}) ->
    [<<?CHANNEL_MESSAGE, Chan:16/binary>>, encode_obj(Vsn, Body)];
encode_message(Vsn, #channel_close{id=Chan, body=Body}) ->
    [<<?CHANNEL_CLOSE, Chan:16/binary>>, encode_obj(Vsn, Body)].

encode_channel_type(sub) -> $S;
encode_channel_type(call) -> $C.

encode_obj(D) ->
    iolist_to_binary(encode_obj(?latest_vsn, D)).

%% Tagged types
encode_obj(Vsn, ?raw(Data)) -> mmd_decode:downgrade_body(Vsn, Data);
encode_obj(v1_1, Obj) when is_binary(Obj) ->
    Sz = iolist_size(Obj),
    mk_tag_with_size(?FAST_STRING, Sz, Obj);
encode_obj(v1_1, 0) -> <<?UINT0>>;
encode_obj(v1_1, Obj) when is_integer(Obj) ->
    case Obj of
        O when O > 0 andalso O < 256 -> <<?UINT1, O:8>>;
        O when O >= -128 andalso O < 128 -> <<?INT1, O:8/signed>>;
        O when O > 0 andalso O < 65536 -> <<?UINT2, O:16>>;
        O when O >= -32768 andalso O < 32768 -> <<?INT2, O:16/signed>>;
        O when O > 0 andalso O < 4294967296 -> <<?UINT4, O:32>>;
        O when O >= -2147483648 andalso O < 2147483648 ->
            <<?INT4, O:32/signed>>;
        O when O > 0 andalso O < 18446744073709551616 -> <<?UINT8, O:64>>;
        O when O >= -9223372036854775808 andalso
               O < 9223372036854775808 ->
            <<?INT8, O:64/signed>>;
        O -> ?lerr("Int too big to mmd encode: ~p", [O]), O
    end;
encode_obj(_, Obj) when is_float(Obj) ->
    <<?DOUBLE, Obj:64/float>>;

encode_obj(_, true) -> <<?TRUE>>;
encode_obj(_, false) -> <<?FALSE>>;
encode_obj(_, undefined) -> <<?NULL>>;
encode_obj(_, null) -> <<?NULL>>;
encode_obj(_, nil) -> <<?NULL>>;

encode_obj(Vsn, ?map(Data)) ->
    {Sz, Body} = encode_map(Vsn, Data),
    case Vsn of
        v1_1 -> mk_tag_with_size(?FAST_MAP, Sz, Body);
        v1_0 -> [?VARINT_MAP, encode_varint(Sz), Body]
    end;
encode_obj(Vsn, ?array(Data)) ->
    {Sz, Body} = encode_array(Vsn, Data),
    case Vsn of
        v1_1 -> mk_tag_with_size(?FAST_ARRAY, Sz, Body);
        v1_0 -> [?VARINT_ARRAY, encode_varint(Sz), Body]
    end;
encode_obj(v1_1, ?error(Code, Msg)) ->
    mk_tag_with_size(?FAST_ERROR,
                     Code,
                     iolist_to_binary(encode_obj(v1_1, Msg)));
encode_obj(v1_1, ?bytes(Data)) ->
    Data2 = iolist_to_binary(Data),
    mk_tag_with_size(?FAST_BYTES, size(Data2), Data2);
encode_obj(_, ?byte(Byte)) ->
    [?BYTE, Byte];
encode_obj(_, ?uuid(Data)) -> [?UUID, encode_uuid(Data)];
encode_obj(_, ?secid(Data)) -> <<?SECID, Data>>;
encode_obj(v1_1,  ?time(Data)) -> <<?FAST_TIME, Data:64>>;

encode_obj(Vsn, Obj) when is_atom(Obj) ->
    encode_obj(Vsn, atom_to_binary(Obj, utf8));
encode_obj(Vsn, Obj) when is_list(Obj) -> 
    encode_obj(Vsn, ?array(Obj));
encode_obj(Vsn, Obj) when is_tuple(Obj) ->
    encode_obj(Vsn, ?array(tuple_to_list(Obj)));

encode_obj(v1_0, Obj) when is_binary(Obj) ->
    [?VARINT_STRING, encode_varint(size(Obj)), Obj];

encode_obj(v1_0, ?error(Code, Msg)) ->
    [?VARINT_ERROR, encode_svarint(Code), encode_obj(v1_0, Msg)];
encode_obj(v1_0, ?bytes(Data)) -> [?VARINT_BYTES, encode_varint(Data), Data];
encode_obj(v1_0,  ?time(Data)) -> [?VARINT_TIME, encode_svarint(Data)];
encode_obj(v1_0, Obj) when is_integer(Obj), Obj < 0 ->
    [?SVARINT64, encode_varint(Obj)];
encode_obj(v1_0, Obj) when is_integer(Obj) ->
    [?VARINT64, encode_varint(Obj)].

mk_tag_with_size(Tag, Sz, Body) ->
    SSz = ssz(Sz),
    [<<Tag, 1:4, SSz:4, Sz:SSz/unsigned-unit:8>>, Body].

ssz(Sz) ->
    case Sz of
        _ when Sz < 1 -> 0;
        _ when Sz < 256 -> 1;
        _ when Sz < 65536 -> 2;
        _ when Sz < 4294967296 -> 4;
        _ when Sz < 18446744073709551616 -> 8
    end.

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
