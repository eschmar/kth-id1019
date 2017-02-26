-module(philosopher).
-compile(export_all).
-record(philo, {hungry, left, right, name, color, gui, ctrl}).

-define(Dream, 400).
-define(Eat, 200).
-define(Timeout, 1000).

%
%   philosopher, 3 states: dream, wait, eat
%

start(Hungry, Left, Right, Name, Color, Ctrl) ->
    Guest = #philo{
        name=Name,
        hungry=Hungry,
        left=Left,
        right=Right,
        color=Color,
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
            % sleep(?Timeout),
            Guest#philo.gui ! stop,
            % Guest#philo.ctrl ! done,
            out("=====================> is happy <===", Guest)
    end.

% think about awesome things
dream(Guest) ->
    out("fell asleep", Guest),
    Guest#philo.gui ! leave,
    sleep(?Dream),
    out("woke up", Guest),
    wait(Guest).

% try to acquire the two chopsticks
wait(Guest) ->
    out("decides to eat", Guest),
    Guest#philo.gui ! waiting,
    out(io_lib:format("requests both chopsticks: [~p & ~p]", [Guest#philo.left, Guest#philo.right]), Guest),
    case chopstick:request(Guest#philo.left, Guest#philo.right, ?Timeout) of
        ok ->
            out("acquired both chopsticks", Guest),
            eat(Guest);
        denied -> dream(Guest)
    end.

% eat noodles
eat(Guest) ->
    out("starts eating", Guest),
    Guest#philo.gui ! enter,
    sleep(?Eat),
    out("finished eating", Guest),

    out(io_lib:format("returns both chopsticks: [~p & ~p]", [Guest#philo.left, Guest#philo.right]), Guest),
    chopstick:return(Guest#philo.left),
    chopstick:return(Guest#philo.right),

    live(Guest#philo{hungry=Guest#philo.hungry-1}).

%
%   helper
%

sleep(Time) ->
    timer:sleep(rand:uniform(Time)).

out(Str, Guest) ->
    Out = io_lib:format("~s ~p ~s", [Guest#philo.name, self(), Str]),
    color:out(Out, Guest#philo.color).

out(Str, Num, Guest) ->
    Out = io_lib:format("~s ~p ~s (~w)", [Guest#philo.name, self(), Str, Num]),
    color:out(Out, Guest#philo.color).