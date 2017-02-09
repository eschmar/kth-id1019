-module(philosopher).
-compile(export_all).
-record(philo, {hungry, left, right, name, ctrl}).

%
%   philosopher, 3 states: dream, wait, eat
%

start(Hungry, Left, Right, Name, Ctrl) ->
    Guest = #philo{
        name=Name,
        hungry=Hungry,
        left=Left,
        right=Right,
        ctrl=Ctrl
    },
    spawn_link(fun() -> init(Guest) end).

init(Guest) ->
    out("joined the table", Guest),
    live(Guest).

live(Guest) ->
    if
        Guest#philo.hungry > 0 ->
            out("---------------------> is still hungry <---", Guest),
            dream(Guest);
        true ->
            out("=====================> has finished eating <===", Guest)
    end.

% think about awesome things
dream(Guest) ->
    out("fell asleep", Guest),
    sleep(),
    out("woke up", Guest),
    wait(Guest).

% try to acquire the two chopsticks
wait(Guest) ->
    out("decides to eat", Guest),
    out("requests left chopstick", Guest),
    chopstick:request(Guest#philo.left),
    out("requests right chopstick", Guest),
    chopstick:request(Guest#philo.right),
    out("acquired both chopsticks", Guest),
    eat(Guest).

% eat noodles
eat(Guest) ->
    out("starts eating", Guest),
    sleep(),
    out("finished eating", Guest),
    out("returns left chopstick", Guest),
    chopstick:return(Guest#philo.left),
    out("returns right chopstick", Guest),
    chopstick:return(Guest#philo.right),
    live(Guest#philo{hungry=Guest#philo.hungry-1}).

%
%   helper
%

sleep() ->
    timer:sleep(timer:seconds(1 + rand:uniform(3))).

out(Str, Guest) -> io:format("~s ~s (~p).~n", [Guest#philo.name, Str, self()]).