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

encode(#channel_create{id=Id,
                       body=Body,
                       auth_token=AT,
                       service=Svc,
                       type=T}) ->
    ATBin = list_to_binary(uuid:to_string(AT)),
    {ok,
     [json_head(T, Id),
      <<"\"service\":\"", Svc/binary, "\",\"token\":\"", ATBin/binary, "\",">>,
      json_body(Body)]};
encode(#channel_message{id=Id, body=Body}) ->
    {ok, [json_head(msg, Id), json_body(Body)]};
encode(#channel_close{id=Id, body=Body}) ->
    {ok, [json_head(close, Id), json_body(Body)]}.

json_head(Type, Id) ->
    TypeBin = p6str:mkbin(Type),
    ChanIdBin = list_to_binary(uuid:to_string(Id)),
    <<"{\"", TypeBin/binary, "\":\"", ChanIdBin/binary, "\",">>.


json_body(?raw(Body)) ->
    {JsonBody, <<>>} = mmd_to_json(Body),
    [<<"\"body\":">>, JsonBody, $}];
json_body(Other) ->
    json_body(?raw(mmd_encode:encode_obj(Other))).


mmd_to_json(<<?DOUBLE, Double:64/float, Rest/binary>>) ->
    {list_to_binary(float_to_list(Double)), Rest};
mmd_to_json(<<?FLOAT, Float:32/float, Rest/binary>>) ->
    {list_to_binary(float_to_list(Float)), Rest};
mmd_to_json(<<?NULL, Data/binary>>) ->
    {<<"null">>, Data};
mmd_to_json(<<?TRUE, Data/binary>>) ->
    {<<"true">>, Data};
mmd_to_json(<<?FALSE, Data/binary>>) ->
    {<<"false">>, Data};
mmd_to_json(<<?UUID, UUID:16/binary, Rest/binary>>) ->
    {[<<"{\"_mmd_uuid\":\"">>, list_to_binary(uuid:to_string(UUID)), <<"\"}">>],
     Rest};
mmd_to_json(<<?SECID, SecId:16/binary, Rest/binary>>) ->
    {security_id:get_key(SecId), Rest};
mmd_to_json(<<?BYTE, B, Rest/binary>>) ->
    {list_to_binary(integer_to_list(B)), Rest};
mmd_to_json(<<?FAST_STRING, Rest/binary>>) ->
    {Sz, Rest2} = mmd_decode:tag_size(Rest),
    <<Str:Sz/binary, Rest3/binary>> = Rest2,
    {[$",
      re:replace(Str, <<"[\"\\\\]">>, <<"\\\\&">>, [{return, binary}, global]),
      $"],
     Rest3};
mmd_to_json(<<1:4, Sz:4, Int:Sz/unsigned-unit:8, Rest/binary>>) ->
    {list_to_binary(integer_to_list(Int)), Rest};
mmd_to_json(<<0:4, Sz:4, Int:Sz/signed-unit:8, Rest/binary>>) ->
    {list_to_binary(integer_to_list(Int)), Rest};
mmd_to_json(<<?FAST_TIME, Time:64/integer, Rest/binary>>) ->
    {[<<"{\"_mmd_time\":">>, list_to_binary(integer_to_list(Time)), $}], Rest};

mmd_to_json(<<?FAST_MAP, Rest/binary>>) ->
    {Sz, Rest2} = mmd_decode:tag_size(Rest),
    mmd_to_json_map(Rest2, Sz, []);
mmd_to_json(<<?FAST_ARRAY, Rest/binary>>) ->
    {Sz, Rest2} = mmd_decode:tag_size(Rest),
    mmd_to_json_array(Rest2, Sz, []);
mmd_to_json(<<?FAST_BYTES, Rest/binary>>) ->
    {Sz, Rest2} = mmd_decode:tag_size(Rest),
    <<Bytes:Sz/binary, Rest3/binary>> = Rest2,
    {mmd_to_json(Bytes), Rest3};
mmd_to_json(<<?FAST_ERROR, Rest1/binary>>) ->
    {Code, Rest2} = mmd_to_json(Rest1),
    {Obj, Rest3} = mmd_to_json(Rest2),
    {[<<"{\"_mmd_error\":">>,
      Code,
      <<",\"msg\":">>,
      Obj,
      $}
     ],
     Rest3}.

mmd_to_json_array(Bin, 0, []) ->
    {<<"[]">>, Bin};
mmd_to_json_array(Bin, 0, Acc) ->
    {iolist_to_binary([$[, lists:reverse(Acc), $]]), Bin};
mmd_to_json_array(Bin, Sz, Acc) ->
    {E, Rem} = mmd_to_json(Bin),
    mmd_to_json_array(Rem,
                      Sz - 1,
                      case Acc of
                          [] -> [E];
                          _ -> [E, $, | Acc]
                      end).

mmd_to_json_map(Bin, 0, []) ->
    {<<"{}">>, Bin};
mmd_to_json_map(Bin, 0, Acc) ->
    {iolist_to_binary([${, Acc, $}]), Bin};
mmd_to_json_map(Bin, Sz, Acc) ->
    {K, VBin} = mmd_to_json(Bin),
    {V, Rem} = mmd_to_json(VBin),
    mmd_to_json_map(Rem,
                    Sz - 1,
                    case Acc of
                        [] -> [K, $:, V];
                        _ -> [K, $:, V, $, | Acc]
                    end).
