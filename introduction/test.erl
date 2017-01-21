-module(test).
-compile(export_all).

%
%   Double function
%

double(N) ->
    2 * N.

%
%   Fahrenheit to Celsius
%

fahrenheit2celsius(F) ->
    (F - 32) / 1.8.

%
%   Area functions
%

area({rectangle, Height, Width}) ->
    Height * Width;

area({square, Side}) ->
    area({rectangle, Side, Side});

area({circle, Radius}) ->
    math:pi() * Radius * Radius.

%
%   Recursion
%

product(0, _) ->
    0;
product(M, N) ->
    N + product(M - 1, N).

exp(X, 0) ->
    1;
exp(X, Y) ->
    product(X, exp(X, Y - 1)).

exp2(X, 1) ->
    X;
exp2(X, N) ->
    case N rem 2 of
        0 -> exp2(X, N div 2) * exp2(X, N div 2);
        1 -> exp2(X, N - 1) * X
    end.
