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
encode_obj(?raw(Data)) ->
    Data;
encode_obj(?map(Data)) ->
    [?VARINT_MAP,encode_map(Data)];
encode_obj(?array(Data)) ->
    [?VARINT_ARRAY,encode_array(Data)];
encode_obj(?error(Code,Msg)) ->
    [?VARINT_ERROR,encode_type(svarint32,Code),encode_obj(Msg)];
encode_obj(?bytes(Data)) ->
    [?VARINT_BYTES,encode_type(bytes,Data)];
encode_obj(?byte(Byte)) ->
    [?BYTE,Byte];
encode_obj(?uuid(Data)) ->
    [?UUID,encode_type(uuid,Data)];
encode_obj(?secid(Data)) ->
    [?SECID,encode_type(secid,Data)];
encode_obj(?time(Data)) ->
    [?VARINT_TIME,encode_type(date,Data)];
encode_obj(?NAN) ->
    [?DOUBLE,?DOUBLE_NAN];
encode_obj(?INFINITY) ->
    [?DOUBLE,?DOUBLE_INF];
encode_obj(?NEGINFINITY) ->
    [?DOUBLE,?DOUBLE_NEGINF];
%% exact types
encode_obj(true) -> [?TRUE];
encode_obj(false) -> [?FALSE];
encode_obj(undefined) -> [?NULL];
encode_obj(null) -> [?NULL];
encode_obj(nil) -> [?NULL];
%% inferred types
encode_obj(Obj) when is_pid(Obj) ->
    encode_obj(p6str:mkbin(Obj));
encode_obj(Obj) when is_integer(Obj), Obj < 0 ->
    [?SVARINT64,encode_type(svarint64,Obj)];
encode_obj(Obj) when is_integer(Obj) ->
    [?VARINT64,encode_type(varint64,Obj)];
encode_obj(Obj) when is_float(Obj) ->
    [?DOUBLE,encode_type(double,Obj)];
encode_obj(Obj) when is_atom(Obj) ->
    [?VARINT_STRING,encode_type(string,Obj)];
encode_obj(Obj) when is_binary(Obj) ->
    [?VARINT_STRING,encode_type(string,Obj)];
encode_obj(Obj) when is_list(Obj) ->
    [?VARINT_ARRAY,encode_type(array,Obj)];
encode_obj(Obj) when is_tuple(Obj) ->
    [?VARINT_ARRAY,encode_type(array,Obj)].

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
encode_type(boolean,true) ->
    <<$T>>;
encode_type(boolean,false) ->
    <<$F>>;
encode_type(array,Arr) when is_tuple(Arr) ->
    encode_type(array,tuple_to_list(Arr));
encode_type(array,List) when is_list(List) ->
    encode_array(List);
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
encode_type(bytes,Bytes) when is_binary(Bytes)->
    [encode_varint(size(Bytes)),Bytes];
encode_type(string,String) when is_tuple(String) ->
    encode_type(string,p6str:mkstr("~p",String));
encode_type(string,String) when is_list(String) ->
    encode_type(string,list_to_binary(String));
encode_type(string,String) when is_atom(String) ->
    encode_type(string,atom_to_binary(String,utf8));
encode_type(string,String) when is_binary(String)->
    [encode_varint(size(String)),String].

encode_array(Data) ->
    encode_array(Data,0,[]).

encode_array([],Sz,Acc) ->
    [encode_type(varint32,Sz),lists:reverse(Acc)];
encode_array([Item|Items],Sz,Acc) ->
    encode_array(Items,Sz+1,[encode_obj(Item)|Acc]).

encode_map(Data) ->
    encode_map(Data,0,[]).

encode_map([],Sz,Acc) ->
    [encode_varint(Sz),Acc];
encode_map([{K,V}|Tail],Sz,Acc) ->
    encode_map(Tail,Sz+1,[encode_obj(K),encode_obj(V)|Acc]).

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
