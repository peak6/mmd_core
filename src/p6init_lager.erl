-module(p6init_lager).

-include("p6init.hrl").

-export([initLager/2]).

-define(lset(K,V), application:set_env(lager,K,V)).

initLager(Env,_Props) ->
    application:load(sasl),
    application:set_env(sasl,errlog_type,error),
    application:load(lager),
    case application:get_env(mmd_core,log_dir) of
        {ok,LogDir} -> ok;
        _ -> LogDir = "log"
    end,
    ?lset(crash_log_count,5),
    ?lset(included_applications,[]),
    ?lset(crash_log_date,"$D0"),
    ?lset(crash_log_size,0),
    ?lset(colored,true),
    ?lset(crash_log_msg_size,65536),
    ?lset(crash_log,p6str:mkstr("~s/~s.crash.log",[LogDir,node()])),
    case application:get_env(mmd_core,log_console) of
        {ok,true} -> Console = [{lager_console_backend,debug}];
        _ -> Console = []
    end,
    
    ?lset(handlers,
          Console ++ 
	      [{lager_file_backend,[
				    {file,p6str:mkstr("~s/~s.server.log",[LogDir,node()])},
				    {level,debug}
				   ]}
	      ]),
    ?lset(error_logger_redirect,true),
    Env.

