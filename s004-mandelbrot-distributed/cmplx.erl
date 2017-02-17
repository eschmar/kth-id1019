-module(cmplx).

-export([new/2,add/2, sqr/1, abs/1]).


new(X, Y) ->
    {X, Y}.

add({X1,Y1}, {X2,Y2}) ->
    {X1+X2, Y1+Y2}.

sqr({X,Y}) ->
    {X*X - Y*Y, 2*X*Y}.

abs({X,Y}) ->
    math:sqrt(X*X+Y*Y).

