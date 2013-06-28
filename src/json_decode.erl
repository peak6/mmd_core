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
-module(json_decode).

-export([decode/1]).
-export([decodeObj/1]).
-export([parseTime/1]).
-include_lib("p6core/include/logger.hrl").

-include("mmd.hrl").

decodeObj({json,Json}) -> decodeObj(Json);
decodeObj(JS) ->
    case json:decode(JS) of
        {ok,Obj,[]} -> catch body(Obj);
        E -> E
    end.

decode({json,Json}) -> decode(Json);
decode(Json) ->
    {ok,Obj,[]} = json:decode(Json),
    catch decodeDoc(Obj).

decodeDoc({obj,Map}) ->
    case p6props:first(["call","sub","close","msg"],Map) of
        undefined -> {error,invalid_mmd_msg};
        Other -> transDoc(Other,p6props:any(["svc","auth","body","timeout"],Map))
    end.

transDoc(undefined,[_,_,_,_]) ->
    {error,missing_action};
transDoc({"close",Id},[_,_,Body,_]) ->
    #channel_close{id=uuid(Id),body=body(Body)};
transDoc({"msg",Id},[_,_,Body,_]) ->
    #channel_message{id=uuid(Id),body=body(Body)};
transDoc(_,[undefined,_,_,_]) ->
    {error,missing_service};
transDoc({"call",Id},[Svc,AT,Body,Timeout]) ->
    #channel_create{type=call,id=uuid(Id),service=svc(Svc),auth_token=auth(AT),body=body(Body),timeout=transTimeout(Timeout)};
transDoc({"sub",Id},[Svc,AT,Body,Timeout]) ->
    #channel_create{type=sub,id=uuid(Id),service=svc(Svc),auth_token=auth(AT),body=body(Body),timeout=transTimeout(Timeout)}.


auth(undefined) -> ?NO_AUTH;
auth(Other) -> uuid(Other).

uuid(?uuid(U)) -> U;
uuid(Id) ->
    try
        p6uuid:parse(Id)
    catch
        error:function_clause -> throw({error,{bad_uuid,Id}});
        T:E -> throw({error,{T,E}})
    end.

body(Body) -> toErl(Body).
svc(Bin) -> p6str:to_lower_bin(Bin).

transTimeout(undefined) -> 3000;
transTimeout(Time) -> Time.

toErl(List) when is_list(List) -> ?array(lists:map(fun(X) -> toErl(X) end, List));
toErl(Num) when is_number(Num) -> Num;
%%toErl({obj,[{"_mmd_uuid",UUID}]}) -> ?uuid(UUID);%
%toErl({obj,[{"_mmd_secid",SecID}]}) -> ?secid(SecID);
%toErl({obj,[{"_mmd_time",Time}]}) -> parseTime(Time);
toErl({obj,Obj}) -> transMap(Obj);
		      %%?map(lists:map(fun({K,V}) -> {list_to_binary(K),toErl(V)} end, Obj)));
toErl(null) -> undefined;
toErl(Val) -> Val.

%% Special MMD types that json doesn't handle are sent as maps with a special key
transMap([{"_mmd_uuid",UUID}]) -> ?uuid(UUID);
transMap([{"_mmd_secid",SecID}]) -> ?secid(SecID);
transMap([{"_mmd_time",Time}]) -> parseTime(Time);
transMap(Map) ->
    case p6props:all([<<"_mmd_error">>,<<"msg">>],Map) of
	{ok,Code,Msg} -> ?error(Code,toErl(Msg));
	%% Map isn't an error, just translate the k/v pairs
	_ -> ?map(lists:map(fun({K,V}) -> {list_to_binary(K),toErl(V)} end, Map))
    end.

% transMap(M) ->
% %% We're holding a map, first see if it's an error message
%     transError(M).

% transError(M=?map(Map)) ->
% %% If _mmd_error is not present, try to process as a UUID / SecID
%     case p6props:any([<<"_mmd_error">>,<<"msg">>],Map) of
%         [undefined,_] -> transUUID(M);
%         [Code,Msg] -> ?error(Code,Msg)
%     end.

% transUUID(M=?map(Map)) ->
%     case p6props:any([<<"_mmd_uuid">>,<<"_mmd_secid">>],Map) of
%         [undefined,undefined] -> M;
%         [undefined,SecID] -> ?secid(uuid(SecID));
%         [UUID,undefined] -> ?uuid(uuid(UUID))
%     end.


parseTime(Time) when is_binary(Time) -> parseTime(binary_to_list(Time));
parseTime(Time) when is_integer(Time) -> ?time(Time);
parseTime(Date) when is_list(Date) andalso length(Date) =< 10 ->
    ?time(p6time:convert(list_to_tuple(lists:map(fun list_to_integer/1, string:tokens(Date,"/"))),date,us)).




