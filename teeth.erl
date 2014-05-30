-module(teeth).
-export([alert/1]).

alert(PocketDepths) -> alert(PocketDepths, 1, []).

alert([], _, BadTeeths) -> lists:reverse(BadTeeths);
alert([H|T], Inc, BadTeeths) -> 
    case ch6:maximum(H) >= 4 of 
        true -> alert(T, Inc + 1, [Inc | BadTeeths]);
        _ -> alert(T, Inc + 1, BadTeeths)
   end.
