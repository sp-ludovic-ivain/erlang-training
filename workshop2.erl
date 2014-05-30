-module(workshop2).
-export([lc_quicksort/1, empty/0, insert/3, lookup/2]).

lc_quicksort([]) -> [];
lc_quicksort([Pivot|Rest]) ->
    lc_quicksort([Smaller || Smaller <- Rest, Smaller =< Pivot])
        ++ [Pivot] ++
    lc_quicksort([Larger || Larger <- Rest, Larger > Pivot]).



empty() -> {node, 'nil'}.

insert(Key, Val, {node, 'nil'}) ->
    {node, {Key, Val, empty(), empty()}};
insert(NewKey, NewVal, {node, {Key, Val, Smaller, Larger}}) when NewKey < Key -> {node, {Key, Val, insert(NewKey, NewVal, Smaller), Larger}};
insert(NewKey, NewVal, {node, {Key, Val, Smaller, Larger}}) when NewKey > Key -> {node, {Key, Val, Smaller, insert(NewKey, NewVal, Larger)}};
insert(Key, NewVal, {node, {Key, _, Smaller, Larger}}) ->
    {node, {Key, NewVal, Smaller, Larger}}.

lookup(_, {node, 'nil'}) ->
        undefined;
lookup(Key, {node, {Key, Val, _, _}}) ->
        {ok, Val};
lookup(Key, {node, {NodeKey, _, Smaller, _}}) when Key < NodeKey ->
        lookup(Key, Smaller);
lookup(Key, {node, {_, _, _, Larger}}) ->
        lookup(Key, Larger).


