-module(json).

-export([decode/1,encode/1]).


decode(JSON) ->
    rfc4627:decode_noauto(JSON).

encode(Obj) ->
    rfc4627:encode_noauto(Obj).





