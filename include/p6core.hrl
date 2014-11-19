-ifndef(P6CORE_HRL_).
-define(P6CORE_HRL_,1).
-include("logger.hrl").

-define(DUMP_REC(RecName,Rec), {RecName,record_infograbber:getInfo(all,Rec,record_info(fields,RecName))}).

-define(atom(Term), p6str:mkatom(Term)).
-define(atom(Fmt,Args), p6str:mkatom(Fmt,Args)).

-endif. %% P6CORE_HRL_
