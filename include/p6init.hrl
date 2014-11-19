-define(DEBUG,2).
-define(INFO,1).
-define(ALL,100).

-record(env,{curItem,
             file,env=p6init:getEnv(),
             app=undefined,
             log=?INFO,
             entries=[],
             loaded=[],
             noexit=false,
             paths=[filename:absname(F)
                    || F <- [okget:ok(file:get_cwd()),"priv"]
                           ++ [re:replace(P,"/ebin$","/priv",[{return,list}])
                               || P <- code:get_path()] ]}).

-define(log(Env,Level,Msg,Args), p6init_log:log(Env,Level,Msg,Args)).
-define(linfo(Env,Msg,Args), ?log(Env,?INFO,Msg,Args)).
-define(linfo(Env,Msg), ?linfo(Env,Msg,[])).

-define(ldebug(Env,Msg,Args), ?log(Env,?DEBUG,Msg,Args)).
-define(ldebug(Env,Msg), ?linfo(Env,Msg,[])).

-define(die(Env,Msg,Args), p6init_log:die(Env,Msg,Args)).
-define(die(Env,Msg), ?die(Env,Msg,[])).

-define(loadFile(Env), p6init_file:loadFile(Env)).
-define(loadFile(Env,File), p6init_file:loadFile(Env,File)).

-define(TABLE,p6init).
