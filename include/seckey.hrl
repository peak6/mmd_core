-type callput() :: 'call' | 'put'.

-record(seckey, {type :: atom(),
		 sym :: string(),
		 expdate :: string(),
		 strike :: float(),
		 callput :: callput()}
       ).
