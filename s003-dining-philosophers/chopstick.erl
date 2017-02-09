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
            gone();
        quit ->
            ok
    end.

gone() ->
    out("Chopstick gone"),
    receive
        {return, From} ->
            From ! released,
            available();
        quit ->
            ok
    end.

%
%   api
%

request(Stick) ->
    Stick ! {request, self()},
    receive
        granted ->
            ok
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