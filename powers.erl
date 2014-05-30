-module(powers).
-export([raise/2, raise/3, nth_root/2, nth_root/3]).

raise(_, 0) -> 1;

raise(X, 1) -> X;

raise(X, Y) when Y > 0 ->
    io:format("X= ~p, Y= ~p ~n", [X, Y]),
    Res = raise(X, Y, 1),
    io:format("Result = ~p ~n", [Res]),
    Res;

raise(X, Y) when Y < 0 -> 1.0 / raise(X, -Y);

raise(_, _) -> 0.

raise(_, 0, Accu) -> Accu;

raise(X, Y, Accu) -> raise(X, Y - 1, X * Accu).

nth_root(X, Y) -> nth_root(X, Y, X / 2.0).

nth_root(X, Y, Approx) -> 
    io:format("Current approx is ~p ~n", [Approx]),
    F = raise(Approx, Y) - X,
    Fp = Y * raise(Approx, Y - 1),
    Next = Approx - (F / Fp),
    Change = abs(Next - Approx),
    if
        Change < 1.0e-8 -> Next;
        true -> nth_root(X, Y, Next)
    end.
