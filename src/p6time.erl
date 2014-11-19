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
-module(p6time).

-export([nowAs/1]).
-export([fromNow/2]).
-export([timeLeft/2]).
-export([convert/3]).
-export([normalize/1]).

-include("time.hrl").
-include("logger.hrl").

-type msu() :: {Mega::integer(),Sec::integer(),Micro::integer()}.
-type timeType() :: mega|s|ms|us|msu.
-type time() :: integer() | msu().

%%
%% @doc Current time as any timeType().
%%
-spec(nowAs(Type::timeType()) -> time()).
nowAs(Type) -> convert(os:timestamp(),msu,Type).

%%
%% @doc Computes N timeType() units from now.
%%
-spec(fromNow(Type::timeType(),N::integer()) -> integer()).
fromNow(msu,{M,S,U}) ->
	{NM,NS,NU} = nowAs(msu),
	normalize(M+NM, S+NS, U+NU);

fromNow(Type,N) -> nowAs(Type)+N.

timeLeft(Type,EndTime) -> EndTime - nowAs(Type).

%%
%% @doc Converts Time from timeType() to timeType().
%%
-spec(convert(Time::time(), From::timeType(), To::timeType()) -> time()).
convert(      N, Type, Type) -> N;

convert(      D,     date,       To) -> convert({D,{0,0,0}},datetime,To);
convert(      N,     From,     date) -> {Date,_} = convert(N,From,datetime),
                                         Date;

convert(     DT, datetime,       To) -> convert(
                                          calendar:datetime_to_gregorian_seconds(DT) - ?GREGORIAN_SECONDS_1970,
                                          s,
                                          To);
convert(      N,     From, datetime) -> calendar:now_to_local_time(convert(N,From,msu));

convert(      N,       ms,     mega) -> N div ?MEGA_MS;
convert(      N,       ms,        s) -> N div ?SEC_MS;
convert(      N,       ms,       us) -> N  *  ?MS_US;

convert(      N,       us,     mega) -> N div ?MEGA_US;
convert(      N,       us,        s) -> N div ?SEC_US;
convert(      N,       us,       ms) -> N div ?MS_US;

convert({M,S,U},      msu,     Type) -> convert(M,mega,Type) + convert(S,s,Type) + convert(U,us,Type);

convert(      N,     mega,        s) -> N  *  ?MEGA_SEC;
convert(      N,     mega,       ms) -> N  *  ?MEGA_MS;
convert(      N,     mega,       us) -> N  *  ?MEGA_US;

convert(      N,        s,     mega) -> N div ?MEGA_SEC;
convert(      N,        s,       ms) -> N  *  ?SEC_MS;
convert(      N,        s,       us) -> N  *  ?SEC_US;

convert(      N,     mega,      msu) -> { N, 0, 0 };
convert(      N,        s,      msu) -> normalize(0,N,0);
convert(      N,       ms,      msu) -> normalize(0,0,N * ?MS_US);
convert(      N,       us,      msu) -> normalize(0,0,N);
convert(      N,      sec,       To) -> convert(N,s,To);


convert(      N, From,  sec) -> convert(N,From,s).


normalize({M,S,U}) -> normalize(M,S,U).

normalize(M,S,U) ->
    US = M * ?MEGA_US + S * ?SEC_US + U,
    Mega = US div ?MEGA_US,
    Sec = US rem ?MEGA_US div ?SEC_US,
    Micro = US rem ?SEC_US,
    {Mega,Sec,Micro}.

% normalize(M,S,U) when S >= ?MEGA_SEC -> normalize((M + S) div ?MEGA_SEC, S rem ?MEGA_SEC, U);
% normalize(M,S,U) when U >= ?SEC_US -> normalize(M, (S + U) div ?SEC_US, U rem ?SEC_US);
% normalize(M,S,U) -> {M,S,U}.

