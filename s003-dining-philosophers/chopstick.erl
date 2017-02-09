-module(chopstick).
-compile(export_all).

%
%   chopstick
%

start() ->
    spawn_link(fun() -> ... end).

available() ->
    receive
        ... ->
            :
        quit ->
            ok
    end.

gone() ->
    receive
        ... ->
            :
        quit ->
            ok
    end.