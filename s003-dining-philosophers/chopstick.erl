-module(chopstick).
-compile(export_all).

%
%   chopstick
%

start() ->
    out("Spawn new chopstick"),
    spawn_link(fun() -> available() end).

available() ->
    out("Chopstick available"),
    receive
        {request, From} ->
            From ! granted,
            gone(From);
        quit ->
            ok
    end.

gone(Holder) ->
    out("Chopstick gone"),
    receive
        {return, Holder} ->
            Holder ! released,
            available();
        {return, From} ->
            From ! denied,
            gone(Holder);
        quit ->
            ok
    end.

%
%   api
%

request(Stick, Stick2, Timeout) ->
    Stick ! {request, self()},
    timer:sleep(100),
    Stick2 ! {request, self()},
    case receiveStick(Timeout) of
        ok -> receiveStick(Timeout);
        denied -> denied
    end.    

receiveStick(Timeout) ->
    receive
        granted ->
            ok
    after Timeout -> denied
    end.

return(Stick) ->
    Stick ! {return, self()},
    receive
        released ->
            ok
    end.

quit(Stick) ->
    io:format(" ------------------> ~p terminates ~p.", [self(), Stick]),
    Stick ! quit.

%
%   helper
%

out(Str) -> io:format("~s (~p).~n", [Str, self()]).