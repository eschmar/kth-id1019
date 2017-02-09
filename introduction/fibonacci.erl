-module(fibonacci).
-compile(export_all).

%
%  fibonacci, O(2^N)
%

fib(N) when N > 1 -> fib(N-2) + fib(N-1);
fib(1) -> 1;
fib(0) -> 0.

%
%   fibonacci, dynamic O(n)
%

fibb(0) -> {1, na};
fibb(1) -> {1, 1};
fibb(N) ->
    {N1, N2} = fibb(N-1),
    {N1+N2, N1}.

%
%   benchmarks
%

bench() ->
    Ls = [8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40],
    N = 10,
    Bench = fun(L) ->
        Tn = time(N, fun() -> fib(L) end),
        Tr = time(N, fun() -> fibb(L) end),
        io:format("n: ~4w naive: ~8w us dyn: ~8w us~n", [L, Tn, Tr])
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
