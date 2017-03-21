-module(chopstick).
-compile(export_all).

-define(Delay, 40).

%
%   chopstick
%

start() ->
    out("Spawn new chopstick"),
    spawn_link(fun() -> available() end).

available() ->
    receive
        {request, From} ->
            out("Chopstick gone"),
            From ! {granted, self()},
            gone(From);
        quit ->
            ok
    end.

gone(Holder) ->
    receive
        {return, Holder} ->
            out("Chopstick available"),
            available();
        {return, _} ->
            out("Chopstick return denied!"),
            gone(Holder);
        {request, From} ->
            From ! {denied, self()},
            gone(Holder);
        quit ->
            ok
    end.

%
%   api
%

request(Stick, Stick2, Timeout) ->
    Stick ! {request, self()},
    sleep(?Delay),
    Stick2 ! {request, self()},

    receive
        {granted, FirstStick} ->
            receive
                {granted, _SecondStick} ->
                    ok
            after Timeout -> 
                out(io_lib:format("Didn't receive second stick, return first (~p)", [FirstStick])),
                FirstStick ! {return, self()},
                denied
            end
    after Timeout -> 
        denied
    end.

return(Stick) ->
    Stick ! {return, self()}.

quit(Stick) ->
    io:format(" ------------------> ~p terminates ~p.", [self(), Stick]),
    Stick ! quit.

%
%   helper
%

out(Str) -> io:format("~s (~p).~n", [Str, self()]).

sleep(Time) ->
    timer:sleep(rand:uniform(Time)).