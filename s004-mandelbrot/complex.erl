-module(complex).
-compile(export_all).

%
%   complex
%

new(X, Y) -> {X, Y}.

add({X, Xi}, {Y, Yi}) -> {X+Y, Xi+Yi}.

sqr({X, Y}) -> {math:pow(X, 2) - math:pow(Y, 2), X * Y}.

abs({X, Y}) -> math:sqrt(math:pow(X, 2) + math:pow(Y, 2)).
