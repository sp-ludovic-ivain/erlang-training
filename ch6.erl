-module(ch6).
-export([minimum/1, maximum/1, range/1, julian/1]).

minimum(List) -> 
    [Head | Tail] = List,
    minimum(Tail, Head).

minimum([],Min) -> Min;
minimum([H|T],Min) ->
    case H < Min of
        true -> minimum(T, H);
        false -> minimum(T, Min)
    end.


maximum(List) ->
    [H | T] = List,
    maximum(T, H).

maximum([], Max) -> Max;
maximum([H|T], Max) when H > Max -> maximum(T, H);
maximum([_|T], Max) -> maximum(T,Max).


range(List) ->
    [H|T] = List,
    Min = minimum(T,H),
    Max = maximum(T,H),
    [Min,Max].

julian(Date) ->
    [Y, M, D] = ch5:date_parts(Date),
    DaysPerMonth = day_per_month(Y),
    NbDays = julian(Y, M, D, DaysPerMonth, 0),
    NbDays.

julian(_, _, _, [], Accu) -> Accu;
julian(_, M, D, Days, Accu) when (13 - length(Days)) == M -> Accu + D;
julian(Y, M, D, [NbDays | Rest], Accu) -> julian(Y, M, D, Rest, Accu + NbDays).

is_leap_year(Year) ->
      (Year rem 4 == 0 andalso Year rem 100 /= 0)
      orelse (Year rem 400 == 0).

day_per_month(Y) -> 
    case is_leap_year(Y) of
        true -> [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
        _ -> [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    end.






