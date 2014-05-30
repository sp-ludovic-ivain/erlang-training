%% @author Ludovic Ivain <ludovic.ivain@socialpoint.es> 
%% @doc Functions calculating area.
%% SocialPoint Backend, Inc., 2014
%% @copyright 2014 by Ludovic Ivain
%% @version 0.1

-module(geom).
-export([area/1, area/2, area/3, abso/1]).

-spec(area(number(),number()) -> number()).

%% @doc calculating square area
area(X, Y) -> X * Y.

-spec(area(atom(),number(),number()) -> number()).

%% @doc calculating rectangle area
%%area(rectangle, X, Y) when X >= 0, Y >= 0 -> X * Y;

%% @doc calculating triangle area
%%area(triangle, X, Y) when X >= 0, Y >= 0 -> X * Y / 2.0;

%% @doc calculating ellipse area
%%area(ellipse, X, Y) when X >= 0, Y >= 0 -> math:pi() * X * Y;

area(Type, X ,Y) when X >= 0, Y >= 0 ->
    case Type of
        rectangle -> X * Y;
        triangle -> X * Y / 2.0;
        ellipse -> math:pi() * X * Y
    end;

area(_, _, _) -> 0.

-spec(area({atom(), number(), number()}) -> number()).

%%  @doc calculating area with Tuple
area({Type, X, Y}) -> area(Type,X,Y).


abso(X) when X < 0 -> -X;
abso(X) -> X.
