-include_lib("p6core/include/p6core.hrl").

-define(SERVICE_VERSION,1).
-record(service,{node,name,app,enabled=true,tags,pid,version=?SERVICE_VERSION}).

-define(EXIT_ERROR,1).
-define(EXIT_RESTART,2).

%% Current (internal) codec
-define(CODEC_MAJOR,1).
-define(CODEC_MINOR,0).
-define(CODEC_VERSION,<<?CODEC_MAJOR:8,?CODEC_MINOR:8>>).

-define(CHANNEL_DISPATCH_TIMEOUT,300000). %% 5 minutes
-define(NO_AUTH,<<0:128>>).

-define(TAG_WITH_SIZE(Tag, Sz, Data),
        <<Tag, 0:4, SSz:4, Sz:SSz/signed-unit:8, Data/binary>>).

-type uuid() :: binary().
-type authToken() :: uuid().
-type channelId() :: uuid().
-type secID() :: uuid().
-type serviceName() :: binary().
-type createType() :: 'call' | 'sub'.
-type body() :: {mmd_raw,binary()} | term().

%% Channel messages
-record(channel_create,{id=p6uuid:next() :: channelId(),
                        originator=self() :: pid(),
                        service :: serviceName(),
                        type :: createType(),
                        timeout=0 :: integer(),
                        auth_token=?NO_AUTH :: authToken(),
                        body :: body()
                       }).
-record(channel_message,{id,body}).
-record(channel_close,{id,body}).

%% Structure of the auth token in mongo and in erlang
-record(auth_token,{userName,loginTime,lastAccess,userId,token}).

%% Tagged types (to simulate typing in erlang)
%% Macros for conveneince access to the tagged types
-record('$time',{val}).
-define(time(TimeUS),#'$time'{val=TimeUS}).

-record('$error',{code=0,msg}).
-define(error(Code,Fmt,Args),?error(Code,p6str:mkbin(Fmt,Args))).
-define(error(Code,Msg),#'$error'{code=Code,msg=Msg}).

-record('$map',{data}).
-define(map(PropList),#'$map'{data=PropList}).

-record('$array',{data}).
-define(array(List),#'$array'{data=List}).

-record('$uuid',{data}).
-define(uuid(UUID),#'$uuid'{data=UUID}).

-record('$secid',{data}).
-define(secid(SecID),#'$secid'{data=SecID}).

-record('$byte',{data}).
-define(byte(Byte),#'$byte'{data=Byte}).

-record('$bytes',{data}).
-define(bytes(Bytes),#'$bytes'{data=Bytes}).

-record('$raw',{data}).
-define(raw(Raw),#'$raw'{data=Raw}).


-define(UNKNOWN,0).
-define(SERVICE_NOT_FOUND,1).
-define(IMPROPER_RESPONSE_TYPE,2).
-define(BROKER_CONNECTION_CLOSED,3).
-define(SERVICE_ERROR,4).
-define(UNEXPECTED_REMOTE_CHANNEL_CLOSE,5).
-define(INVALID_REQUEST,6).
-define(AUTHENTICATION_ERROR,7).
-define(CHANNEL_ADMIN_CLOSED,8).
-define(INVALID_CHANNEL,9).
-define(TIMEOUT,10).
-define(SERVICE_BUSY,11).

-define(DOUBLE_NAN,<<16#7F,16#F8,0,0,0,0,0,0>>).
-define(DOUBLE_INF,<<16#7F,16#F0,0,0,0,0,0,0>>).
-define(DOUBLE_NEGINF,<<16#FF,16#F0,0,0,0,0,0,0>>).
-define(INFINITY,'Infinity').
-define(NEGINFINITY,'-Infinity').
-define(NAN,'NaN').

%% See protocol.txt for details of these
%% Message Types
-define(CHANNEL_CREATE,       $c).
-define(VARINT_CHANNEL_CREATE,$C).
-define(CHANNEL_MESSAGE,      $M).
-define(CHANNEL_CLOSE,        $X).

%%Object Types
%%Things with size
-define(FAST_MAP,       $r).
-define(VARINT_MAP,     $m).
-define(FAST_ARRAY,     $a).
-define(VARINT_ARRAY,   $A).
-define(FAST_STRING,    $s).
-define(VARINT_STRING,  $S).
-define(FAST_ERROR,     $e).
-define(VARINT_ERROR,   $E).
-define(FAST_BYTES,     $q).
-define(VARINT_BYTES,   $b).


%% Driver level ack, used by socket handler
-define(ACK,     $a).

-define(PING,    $P).
-define(PONG,    $p).

%%Scalars
-define(INT0,     16#00).
-define(INT1,     16#01).
-define(INT2,     16#02).
-define(INT4,     16#04).
-define(INT8,     16#08).
-define(UINT0,    16#10).
-define(UINT1,    16#11).
-define(UINT2,    16#12).
-define(UINT4,    16#14).
-define(UINT8,    16#18).
-define(SVARINT64,   $L).
-define(VARINT64,    $l).
-define(SVARINT32,   $I).
-define(VARINT32,    $i).
-define(DOUBLE,      $D).
-define(FLOAT,       $d).
-define(NULL,        $N).
-define(TRUE,        $T).
-define(FALSE,       $F).
-define(UUID,        $U).
-define(SECID,       $$).
-define(BYTE,        $B).
-define(VARINT_TIME, $#).
-define(FAST_TIME,   $z).

%% vim: ts=4:sts=4:sw=4:et:sta:
