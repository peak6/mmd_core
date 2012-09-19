-module(mmd_bind).

-export([ip/0]).
-export([ip/1]).
-export([ip/2]).

ip() -> ip(bind).
ip(Name) -> ip(application:get_application(),Name).
ip(undefined,_) -> {0,0,0,0};
ip({ok,App},Name) -> ip(App,Name);
ip(App,Name) -> resolve(application:get_env(App,Name)).

resolve(undefined) -> {0,0,0,0};
resolve({ok,Addr}) -> resolve(Addr);
resolve(Addr) ->
    {ok,IP} = inet:ip(Addr),
    IP.


