-module(record_infograbber).
-export([getInfo/3]).


valueOf(_,_,_,[]) -> invalid_field;
valueOf(Field,Rec,Count,[Field|_Fields]) -> element(Count,Rec);
valueOf(Field,Rec,Count,[_|Fields]) -> valueOf(Field,Rec,Count+1,Fields).

getInfo(all,Rec,RecFields) -> getInfo(RecFields,Rec,RecFields);
getInfo(Field,Rec,RecFields) when is_atom(Field) -> valueOf(Field,Rec,2,RecFields);
getInfo(Fields,Rec,RecFields) -> [{F,valueOf(F,Rec,2,RecFields)} || F <- Fields].

    
    
    
