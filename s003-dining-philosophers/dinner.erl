-module(dinner).
-compile(export_all).

%
%   dinner
%

start() ->
    spawn(fun() -> init() end).

init() ->
    C1 = chopstick:start(),
    C2 = chopstick:start(),
    C3 = chopstick:start(),
    C4 = chopstick:start(),
    C5 = chopstick:start(),
    Ctrl = self(),
    philosopher:start(5, C1, C2, "Arendt", Ctrl),
    philosopher:start(5, C2, C3, "Hypatia", Ctrl),
    philosopher:start(5, C3, C4, "Simone", Ctrl),
    philosopher:start(5, C4, C5, "Elizabeth", Ctrl),
    philosopher:start(5, C5, C1, "Ayn", Ctrl),
    wait(5, [C1, C2, C3, C4, C5]).

wait(0, Chopsticks) ->
    lists:foreach(fun(C) -> chopstick:quit(C) end, Chopsticks);

wait(N, Chopsticks) ->
    receive
        done ->
            wait(N-1, Chopsticks);
        abort ->
            exit(abort)
    end.