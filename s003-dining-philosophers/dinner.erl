-module(dinner).
-compile(export_all).

%
%   dinner
%

start(Hungry) ->
    spawn(fun() -> init(Hungry) end).

init(Hungry) ->
    C1 = chopstick:start(),
    C2 = chopstick:start(),
    C3 = chopstick:start(),
    C4 = chopstick:start(),
    C5 = chopstick:start(),
    Ctrl = self(),
    philosopher:start(Hungry, C1, C2, "0 > Arendt", Ctrl),
    philosopher:start(Hungry, C2, C3, "1 > Hypatia", Ctrl),
    philosopher:start(Hungry, C3, C4, "2 > Simone", Ctrl),
    philosopher:start(Hungry, C4, C5, "3 > Elizabeth", Ctrl),
    philosopher:start(Hungry, C5, C1, "4 > Ayn", Ctrl),
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