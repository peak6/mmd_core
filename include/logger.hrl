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

-ifndef(LOGGER_HRL).
-define(LOGGER_HRL,true).

-compile([{parse_transform, lager_transform}]).

-define(LOG_TRACE,1).
-define(LOG_DEBUG,2).
-define(LOG_INFO,3).
-define(LOG_WARN,4).
-define(LOG_ERROR,5).


-define(ltrace(Fmt,Args), lager:trace(Fmt,Args)).
-define(ltrace(Text), ?ltrace(Text,[])).

-define(ldebug(Fmt,Args), lager:debug(Fmt,Args)).
-define(ldebug(Text), ?ldebug(Text,[])).

-define(lwarn(Fmt,Args), lager:warning(Fmt,Args)).
-define(lwarn(Text), ?lwarn(Text,[])).

-define(linfo(Fmt,Args), lager:info(Fmt,Args)).
-define(linfo(Text), ?linfo(Text,[])).

-define(lerr(Fmt,Args), lager:error(Fmt,Args)).
-define(lerr(Text), ?lerr(Text,[])).

-define(lprog(Fmt,Args), lager:progress(Fmt,Args)).
-define(lprog(Text), ?lprog(Text,[])).

-endif.
