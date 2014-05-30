-module(sum).
-export([sum/1, range/1, test_range/1]).

sum([]) -> 0;
sum([Head|Tail]) ->
    sum(Tail, Head).

range(0) -> [];
range(1) -> [1];
range(N) -> 
    range(N, []).

range(0, List) -> List;
range(N, List) ->
    range(N - 1, [N] ++ List).

sum([], Acc) -> Acc;
sum([N], Acc) -> Acc + N;
sum([Head|Tail], Acc) -> sum(Tail, Acc + Head).


test_range(N) -> 
    Seq = lists:seq(1, N),
    Seq = range(N),
    ok.
