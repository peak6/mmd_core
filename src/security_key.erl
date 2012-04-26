-module(security_key).
-export([seckey_parts/1,seckey_type/1, seckey_sym/1]).

-include("seckey.hrl").

seckey_parts(<<"O:",Rem/binary>>) when size(Rem) > 10 ->
    [Sym,YYYYMMDD,Strk,CP] = re:split(Rem,<<":">>),
    CallPut = case CP of
                  <<"C">> -> call;
                  <<"P">> -> put
              end,
    #seckey{type = option,
	    sym = binary:bin_to_list(Sym),
	    expdate = binary:bin_to_list(YYYYMMDD),
	    strike = bin_to_num(Strk),
	    callput = CallPut};

seckey_parts(<<"S:",Sym/binary>>) when size(Sym) > 0 ->
    #seckey{type = stock,
	    sym = binary:bin_to_list(Sym)};

seckey_parts(<<"F:",Sym/binary>>) when size(Sym) > 2 ->
    #seckey{type = future,
	    sym = binary:bin_to_list(Sym)}.

seckey_type(Seckey) ->
    #seckey{type=Type} = seckey_parts(Seckey),
    Type.

seckey_sym(Seckey) ->
    #seckey{sym=Sym} = seckey_parts(Seckey),
    Sym.

bin_to_num(Bin) ->
    N = binary_to_list(Bin),
    case string:to_float(N) of
        {error,no_float} -> list_to_integer(N);
        {F,_Rest} -> F
    end.

