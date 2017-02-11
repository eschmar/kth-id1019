-module(philosopher).
-compile(export_all).
-record(philo, {hungry, left, right, name, gui, ctrl}).

-define(SleepEat, 50).
-define(SleepDream, 500).
-define(SleepWait, 1000).
-define(SleepWaitDelay, 100).

%
%   philosopher, 3 states: dream, wait, eat
%

start(Hungry, Left, Right, Name, Ctrl) ->
    Guest = #philo{
        name=Name,
        hungry=Hungry,
        left=Left,
        right=Right,
        gui=gui:start(Name),
        ctrl=Ctrl
    },
    spawn_link(fun() -> init(Guest) end).

init(Guest) ->
    out("joined the table", Guest),
    live(Guest).

live(Guest) ->
    if
        Guest#philo.hungry > 0 ->
            out("---------------------> is still hungry <---", Guest#philo.hungry, Guest),
            dream(Guest);
        true ->
            Guest#philo.gui ! abort,
            sleep(1000),
            Guest#philo.gui ! stop,
            out("=====================> is happy <===", Guest)
    end.

% think about awesome things
dream(Guest) ->
    out("fell asleep", Guest),
    Guest#philo.gui ! leave,
    Time = ?SleepDream + (rand:uniform(8) * 100),
    sleep(Time),
    out("woke up", Guest),
    wait(Guest).

% try to acquire the two chopsticks
wait(Guest) ->
    out("decides to eat", Guest),
    Guest#philo.gui ! waiting,
    out("requests left chopstick", Guest),
    case chopstick:request(Guest#philo.left, Guest#philo.right, ?SleepWait) of
        ok -> ok;
        denied -> dream(Guest)
    end,
    
    out("acquired both chopsticks", Guest),
    eat(Guest).

% eat noodles
eat(Guest) ->
    out("starts eating", Guest),
    Guest#philo.gui ! enter,
    sleep(?SleepEat),
    out("finished eating", Guest),
    out("returns left chopstick", Guest),
    chopstick:return(Guest#philo.left),
    out("returns right chopstick", Guest),
    chopstick:return(Guest#philo.right),
    live(Guest#philo{hungry=Guest#philo.hungry-1}).

%
%   helper
%

sleep(Time) ->
    timer:sleep(Time).

out(Str, Guest) -> io:format("~s ~s (~p).~n", [Guest#philo.name, Str, self()]).
out(Str, Num, Guest) -> io:format("~s ~s (~w) (~p).~n", [Guest#philo.name, Str, Num, self()]).