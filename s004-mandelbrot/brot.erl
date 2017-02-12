-module(brot).
-compile(export_all).

%
%   brot
%

mandelbrot(C, M) ->
    Z0 = complex:new(0, 0),
    I = 0,
    test(I, Z0, C, M).

test(I, Z, C, M) ->
    Absolute = complex:abs(Z),
    if
        M == I -> 0;
        2 < Absolute -> I;
        true -> test(I+1, complex:add(complex:sqr(Z), C), C, M)
    end.

%
%   tests
%

testImage() ->
    ppm:write("pew.jpg", [
        [{255,0,0},{255,0,0},{255,0,0},{255,0,0},{255,0,0},{255,0,0},{255,0,0},{255,0,0}],
        [{0,255,0},{0,255,0},{0,255,0},{0,255,0},{0,255,0},{0,255,0},{0,255,0},{0,255,0}],
        [{0,0,255},{0,0,255},{0,0,255},{0,0,255},{0,0,255},{0,0,255},{0,0,255},{0,0,255}]
    ]).