-module(mmd_encode_test).

-include_lib("eunit/include/eunit.hrl").
-include("mmd.hrl").
-compile([export_all]).
-define(BIN(B),iolist_to_binary(B)).
-define(TCHAN,<<1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16>>).
-define(TAUTH,<<11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26>>).

encode_channel_close_test() ->
    ?assertEqual(
       <<?CHANNEL_CLOSE,?TCHAN/binary,?INT1,5>>,
       ?BIN(mmd_encode:encode_message(#channel_close{id=?TCHAN,body=5}))).

encode_channel_create_call_test() ->
    ?assertEqual(
       <<?VARINT_CHANNEL_CREATE,?TCHAN/binary,$C,6,"string",0,0,?TAUTH/binary,?INT1,5>>,
       ?BIN(
	  mmd_encode:encode_message(
	    #channel_create{service=string,
			    type=call,
			    timeout=0,
			    auth_token=?TAUTH,
			    id=?TCHAN,
			    body=5}))).
encode_channel_message_test() ->
    ?assertEqual(<<?CHANNEL_MESSAGE,?TCHAN/binary,?INT1,5>>,?BIN(mmd_encode:encode_message(#channel_message{id=?TCHAN,body=5}))).


encode_map_obj_test() ->
    ?assertEqual(<<?MAP,?INT1,2,?STRING,?INT1,7,"string2",?INT1,3,?STRING,?INT1,6,"string",?INT1,5>>,
		 ?BIN(mmd_encode:encode_obj(?map([{string,5},{string2,3}])))).


encode_array_obj_test() ->
    ?assertEqual(<<?ARRAY,?INT1,10,?STRING,?INT1,6,"string">>,
		 ?BIN(mmd_encode:encode_obj(?array([10,string])))).


encode_error_obj_test() ->
    ?assertEqual(<<?ERROR,30,6,"string">>,?BIN(mmd_encode:encode_obj(?error(15,string)))).


encode_bin_uuid_test() ->
    ?assertEqual(<<123:128>>,mmd_encode:encode_uuid(<<123:128>>)).

encode_tagged_uuid_test() ->
    ?assertEqual(<<456:128>>,mmd_encode:encode_uuid(?uuid(<<456:128>>))).

encode_atom_string_test() ->
    ?assertEqual(<<4,"atom">>,?BIN(mmd_encode:encode_type(string,atom))).

encode_list_string_test() ->
    ?assertEqual(<<4,"list">>,?BIN(mmd_encode:encode_type(string,"list"))).

encode_bin_string_test() ->
    ?assertEqual(<<6,"string">>,?BIN(mmd_encode:encode_type(string,<<"string">>))).

encode_varint_test() ->
    ?assertEqual([4],mmd_encode:encode_varint(4)).
encode_svarint_test() ->
    ?assertEqual([8],mmd_encode:encode_svarint(4)).

encode_type_int32_test()->
    ?assertEqual([8],mmd_encode:encode_type(int32,4)).

encode_type_int64_test()->
    ?assertEqual([8],mmd_encode:encode_type(int64,4)).

encode_type_uint32_test()->
    ?assertEqual([4],mmd_encode:encode_type(uint32,4)).

encode_type_uint64_test()->
    ?assertEqual([4],mmd_encode:encode_type(uint64,4)).

encode_type_float_test() ->
    ?assertEqual(<<65,177,153,154>>,mmd_encode:encode_type(float,22.2)).

encode_type_double_test() ->
    ?assertEqual(<<64,54,51,51,51,51,51,51>>,mmd_encode:encode_type(double,22.2)).

