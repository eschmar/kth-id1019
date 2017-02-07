-module(tobinary).
-compile(export_all).

%
%   binary coding (13 -> [1,1,0,1])
%

% Method 1
dec2bin(N) when is_integer(N) and (N >= 0) ->
    dec2bin(N, []).

dec2bin(0, Result) -> Result;
dec2bin(N, Intermediate) ->
    dec2bin(N div 2, [N rem 2 | Intermediate]).

% Method 2, subtract highest power of 2 below N
mergeDec2Bin(N) when is_integer(N) and (N >= 0) ->
    mergeDec2Bin(N, []).

mergeDec2Bin(0, Result) -> lists:reverse(Result);
mergeDec2Bin(N, Intermediate) ->
    {Num, Partial} = power2(N),
    mergeDec2Bin(N - Num, mergeOverride(Intermediate, Partial)).

power2(N) -> power2(N, 1, []).
power2(N, Current, Intermediate) ->
    if
        (Current =< N) and (Intermediate == []) -> power2(N, Current * 2, [1 | Intermediate]);
        (Current =< N) -> power2(N, Current * 2, [0 | Intermediate]);
        true -> {Current div 2, Intermediate}
    end.

mergeOverride(Left, []) -> Left;
mergeOverride([], Right) -> Right;
mergeOverride([_ | LeftTail], [Right | RightTail]) ->
    [Right | mergeOverride(LeftTail, RightTail)].

% Benchmarks
bench() ->
    Ls = [16, 32, 64, 128, 256, 512, 2345, 134675, 24554],
    N = 100,
    Bench = fun(L) ->
        Tn = time(N, fun() -> dec2bin(L) end),
        Tr = time(N, fun() -> mergeDec2Bin(L) end),
        io:format("length: ~10w Method1: ~8w us Method2: ~8w us~n", [L, Tn, Tr])
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