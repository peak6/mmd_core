%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, PEAK6 Investments, L.P.
%%% @doc
%%% Need sane documentation
%%% @end
%%% Created : Mon May 2 2:25pm CDT 2011
%%%-------------------------------------------------------------------
-module(security_id).

-export([get_id/1, compute_id/1]).
-export([get_key/1, compute_key/1]).

-define(STOCK, 0:3).
-define(OPTION, 1:3).
-define(FUTURE, 2:3).

%%--------------------------------------------------------------------
%% @doc
%% Convert a security key to a security ID.  The security key is a
%% string and the security id is 128 bit binary.  The conversion
%% is lossless.  The formats are documented at the top of this file.
%%
%% @spec get_id(string()) -> binary() | error
%% @end
%%--------------------------------------------------------------------

-spec(get_id(SecKey::string()) -> binary() | error).
get_id(SecKey) when is_list(SecKey) -> get_id(list_to_binary(SecKey));
get_id(SecKey) when is_binary(SecKey) ->
    case security_id_cache:getByKey(SecKey) of
        undefined ->
            Id = compute_id(SecKey),

            security_id_cache:store(Id,SecKey),
            Id;
        Id -> Id
    end.

compute_id(<<"O:",Rem/binary>>) when size(Rem) > 10 ->
    [Sym,<<Y:4/binary,M:2/binary,D:2/binary>>,Strk,CP] = re:split(Rem,<<":">>),
    Sz = size(Sym),
    PutCall = case CP of
                  <<"C">> -> 0;
                  <<"P">> -> 1
              end,
    StrkI = trunc(p6str:to_float(Strk)*1000),
    <<?OPTION,PutCall:1,Sz:4,
      (p6str:to_integer(M)):4,
      (p6str:to_integer(D)):5,
      (p6str:to_integer(Y)-1970):7,
      Sym/binary,
      StrkI:32,0:(9-Sz)/unit:8>>;

compute_id(<<"S:",Symbol/binary>>) when size(Symbol) > 0 ->
    Sz = size(Symbol),
    Pad = 15-Sz,
    <<?STOCK,Sz:5,Symbol/binary,0:Pad/unit:8>>;

compute_id(<<"F:",Symbol/binary>>) when size(Symbol) > 2 ->
    Sz = size(Symbol),
    Pad = 15-Sz,
    <<?FUTURE,Sz:5,Symbol/binary,0:Pad/unit:8>>.


%%--------------------------------------------------------------------
%% @doc
%% Convert a security ID to a security key.  The security key is a
%% binary string and the security id is 128 bit binary.  The conversion
%% is lossless.  The formats are documented at the top of this file.
%%
%% @end
%%--------------------------------------------------------------------
-spec(get_key(SecID::binary()) -> binary() | error).
get_key(Id) when is_binary(Id) andalso size(Id) == 16 ->
    case security_id_cache:getById(Id) of
        undefined ->
            K = compute_key(Id),
            security_id_cache:store(Id,K),
            K;
        K -> K
    end.

pad2(N) when N < 10 -> [$0,48+N];
pad2(N) -> integer_to_list(N).

compute_key(<<?OPTION,PC:1,SymSz:4, %% Byte 1
          Month:4,Day:5,Year:7, %% Bytes 2,3
          Symbol:SymSz/binary,
          Strk:32,
          _/binary>>) ->
    iolist_to_binary([$O,$:
                      ,Symbol,$:
                      ,integer_to_list(1970+Year),pad2(Month),pad2(Day),$:
                      ,fmt_strk(Strk),$:
                      ,if PC == 1 -> $P; true -> $C end
                     ]);
%% Stock
compute_key(<<?STOCK,SymSz:5,Symbol:SymSz/binary,_/binary>>) ->
    <<"S:",Symbol/binary>>;

%% Stock
compute_key(<<?FUTURE,SymSz:5,Symbol:SymSz/binary,_/binary>>) ->
    <<"F:",Symbol/binary>>.



fmt_strk(Int) when is_integer(Int) ->
    case lists:reverse(integer_to_list(Int)) of
        [$0,A,B|Rem] -> lists:reverse([A,B,$.|Rem]);
        [A,B,C |Rem] -> lists:reverse([A,B,C,$.|Rem])
    end.

%% vim: ts=4:sts=4:sw=4:et:sta:

