-module(dijkstra).
-export([gcd/2]).

gcd(X, Y) ->
    if
        X == Y -> X;
        X > Y -> gcd(X - Y, Y);
        true -> gcd(X, Y - X)
    end.
