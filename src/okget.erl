-module(okget).

-export([getOrElse/2]).

-export([ok/1]).

getOrElse({ok,Term},_Ignored) -> Term;
getOrElse(_Ignored,Else) -> Else.

ok({ok,Term}) -> Term.
