-module(mmd_sub).
-export([sub/1,sub/2,sub/3]).
-include("mmd.hrl").

sub(Service,Body) -> sub(Service,Body,?NO_AUTH).

sub(Service,Body,AuthToken) ->
    sub(#channel_create{service=Service,auth_token=AuthToken,body=Body,type=sub}).

sub(CC=#channel_create{id=Chan}) ->
    Chan = CC#channel_create.id,
    {ok,CCPid} = client_channel:new(self(),CC),
    {ok, CCPid, Chan}.
