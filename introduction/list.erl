-module(list).
-compile(export_all).

%
%   nth, tail-recursive
%

nth(N, [_ | Tail]) when N > 0 -> 
    nth(N - 1, Tail);
nth(0, [Nth | _]) ->
    Nth;
nth(_, _) ->
    "Invalid index.".

%
%   count, tail-recursive
%

count(L) -> count_acc(L, 0).

count_acc([], Acc) -> Acc;
count_acc([_ | L], Acc) -> count_acc(L, Acc + 1).

%
%   sum, tail-recursive
%

sum(L) -> sum_acc(L, 0).

sum_acc([], Acc) -> Acc;
sum_acc([X | L], Acc) -> sum_acc(L, Acc + X).

%
%   duplicate, both
%

duplicate([]) -> [];
duplicate([X | Tail]) -> [X, X | duplicate(Tail)].

duplicate_tail(L) -> duplicate_acc(L, []).
duplicate_acc([], Acc) -> reverse(Acc);
duplicate_acc([X | Tail], Acc) -> duplicate_acc(Tail, [X, X | Acc]).

reverse(L) -> reverse(L, []).
reverse([], Acc) -> Acc;
reverse([X | Tail], Acc) -> reverse(Tail, [X | Acc]).

%
%   unique
%

lookup(_, []) -> false;
lookup(X, [Current | Tail]) ->
    case Current of
        X -> true;
        _ -> lookup(X, Tail)
    end.

unique(L) -> unique_acc(L, []).
unique_acc([], Acc) -> reverse(Acc);
unique_acc([X | Tail], Acc) ->
    case lookup(X, Acc) of
        false -> unique_acc(Tail, [X | Acc]);
        _ -> unique_acc(Tail, Acc)
    end.

%
%   pack ([a,a,b,c,b,a,c,] -> [[a,a,a], [b],[c,c]])
%

pack(L) -> pack(L, []).
pack([], Acc) -> Acc;
pack([X | Tail], Acc) -> pack(Tail, pack_insert(X, Acc)).

pack_insert(X, []) -> [[X]];
pack_insert(X, [Head | Tail]) ->
    [Current | _] = Head,
    case Current of
        X -> [append(X, Head) | Tail];
        _ -> [Head | pack_insert(X, Tail)]
    end.

append(X, []) -> [X];
append(X, [Head | Tail]) -> [Head | append(X, Tail)].

%
%   insertion sort
%

insert(Element, []) -> [Element];
insert(Element, [Current | Tail]) ->
    if
        Element >= Current -> [Current | insert(Element, Tail)];
        true -> [Element | [Current | Tail]]
    end.

insertionsort(List) -> reverse(insertionsort_acc(List, [])).
insertionsort_acc([], List) -> List;
insertionsort_acc([Current | Tail], Sorted) -> insertionsort_acc(Tail, insert(Current, Sorted)).

%
%   merge sort, unstable
%

mergesort([]) -> [];
mergesort([Head | []]) -> [Head];
mergesort(List) ->
    {Left, Right} = split(List, [], []),
    merge(mergesort(Left), mergesort(Right)).

split([], Left, Right) -> {Left, Right};
split([Head | Tail], Left, Right) -> split(Tail, Right, [Head | Left]).

merge(Left, []) -> Left;
merge([], Right) -> Right;
merge([Left | LeftTail], [Right | RightTail]) ->
    if
        Left < Right -> [Left | merge(LeftTail, [Right | RightTail])];
        true -> [Right | merge([Left | LeftTail], RightTail)]
    end.

%
%   quick sort
%

quicksort([]) -> [];
quicksort([Pivot | Tail]) ->
    {Left, Right} = quick_split(Pivot, Tail, [], []),
    quicksort(Left) ++ [Pivot] ++ quicksort(Right).

quick_split(_, [], Left, Right) ->
    {Left, Right};
quick_split(Pivot, [Current | Tail], Left, Right) ->
    if
        Current < Pivot -> quick_split(Pivot, Tail, append(Current, Left), Right);
        true -> quick_split(Pivot, Tail, Left, append(Current, Right))
    end.


%
%   reverse naive vs tail-recursive
%

bench() ->
    Ls = [16, 32, 64, 128, 256, 512],
    N = 100,
    Bench = fun(L) ->
        S = lists:seq(1,L),
        Tn = time(N, fun() -> nreverse(S) end),
        Tr = time(N, fun() -> reverse(S) end),
        io:format("length: ~10w nrev: ~8w us rev: ~8w us~n", [L, Tn, Tr])
        end,
    lists:foreach(Bench, Ls).

time(N, F)->
    %% time in micro seconds
    T1 = erlang:system_time(micro_seconds),
    loop(N, F),
    T2 = erlang:system_time(micro_seconds),
    (T2 -T1).

loop(N, Fun) ->
    if N == 0 -> ok; true -> Fun(), loop(N-1, Fun) end.

nreverse([]) -> [];
nreverse([H|T]) ->
    R = nreverse(T),
    append(R, [H]).