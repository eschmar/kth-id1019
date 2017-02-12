-module(dinner).
-compile(export_all).

%
%   dinner
%

start(Hungry) ->
    spawn(fun() -> init(Hungry) end).

init(Hungry) ->
    code:add_path("../helper"),
    code:add_path("../introduction"),

    C1 = chopstick:start(),
    C2 = chopstick:start(),
    C3 = chopstick:start(),
    C4 = chopstick:start(),
    C5 = chopstick:start(),
    Ctrl = self(),
    philosopher:start(Hungry, C1, C2, "Arendt", red, Ctrl),
    philosopher:start(Hungry, C2, C3, "Hypatia", green, Ctrl),
    philosopher:start(Hungry, C3, C4, "Simone", yellow, Ctrl),
    philosopher:start(Hungry, C4, C5, "Elizabeth", cyan, Ctrl),
    philosopher:start(Hungry, C5, C1, "Ayn", magenta, Ctrl),
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