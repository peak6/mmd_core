-module(json).

-export([decode/1,encode/1]).


decode(JSON) ->
    rfc4627:decode(JSON).

encode(Obj) ->
    rfc4627:encode(Obj).





