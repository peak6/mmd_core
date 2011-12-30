-module(mmd_decode_test).

-include_lib("eunit/include/eunit.hrl").
-include("mmd.hrl").
-compile([export_all]).
-define(TCHAN,<<1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16>>).
-define(TAUTH,<<11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26>>).

decode_channel_close_test() ->
    ?assertEqual(
       #channel_close{id=?TCHAN,body=?raw(<<?UINT1,5>>)},
       mmd_decode:decode(
	 <<?CHANNEL_CLOSE,?TCHAN/binary,?UINT1,5>>)).

decode_channel_create_call_test() ->
    ?assertEqual(
       #channel_create{type=call,
		       service=string,
		       timeout=0,
		       auth_token=?TAUTH,
		       id=?TCHAN,
		       body=?raw(<<?UINT1,5>>)},
       mmd_decode:decode(<<?VARINT_CHANNEL_CREATE,?TCHAN/binary,$C,6,"string",0,0,?TAUTH/binary,?UINT1,5>>)).

decode_channel_message_test() ->
    ?assertEqual(#channel_message{id=?TCHAN,body=?raw(<<?UINT1,5>>)},
		 mmd_decode:decode(<<?CHANNEL_MESSAGE,?TCHAN/binary,?UINT1,5>>)).


decode_map_obj_test() ->
    Bin = <<?VARINT_MAP,?UINT1,2,?VARINT_STRING,?UINT1,6,"string",?UINT1,5,?VARINT_STRING,?UINT1,7,"string2",?INT1,-2>>,
    io:format("BIN: ~w",[Bin]),
    ?assertEqual(
       ?map([{<<"string">>,5},{<<"string2">>,-2}]),
       decode_obj(Bin)).


decode_array_obj_test() ->
    ?assertEqual(?array([<<"string">>,10]),
		 decode_obj(<<?VARINT_ARRAY,?UINT1,2,?VARINT_STRING,?UINT1,6,"string",?UINT1,10>>)).


decode_error_obj_test() ->
    ?assertEqual(?error(15,<<"string">>),
		 decode_obj(<<?VARINT_ERROR,?UINT1,15,?VARINT_STRING,?UINT1,6,"string">>)).


decode_obj(Bin) ->
    {Obj,<<>>} = mmd_decode:decode_obj(Bin),
    Obj.
