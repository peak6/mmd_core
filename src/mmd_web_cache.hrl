%% cache_time gets 1 second added when a file is minified
-record(cache_entry,{file,mod_local,cache_time,type,content}).
