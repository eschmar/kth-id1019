-module(fibonacci).
-compile(export_all).

%
%  fibonacci, O(2^N)
%

fib(N) when N > 1 -> fib(N-2) + fib(N-1);
fib(1) -> 1;
fib(0) -> 0.

%
%   benchmarks
%

bench() ->
    Ls = [8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40],
    N = 10,
    Bench = fun(L) ->
        T = time(N, fun() -> fib(L) end),
        io:format("n: ~4w fib(n) calculated in: ~8w us~n", [L, T])
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