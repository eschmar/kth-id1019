-module(philosopher).
-compile(export_all).

%
%   philosopher
%

sleep(T,D) ->
    timer:sleep(T + random:uniform(D)).