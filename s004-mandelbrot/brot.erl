-module(brot).
-compile(export_all).

%
%   brot
%

mandelbrot({CR, CI}, M) ->
    I = 0,

    % calculate in c
    complex_nif:depth(I, 0.0, 0.0, CR, CI, M).

    % calculate in erlang
    % Z0 = complex:new(0, 0),
    % C = complex:new(CR, CI),
    % test(I, Z0, C, M).

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
    ppm:write("test.ppm", [
        [{255,0,0},{255,0,0},{255,0,0},{255,0,0},{255,0,0},{255,0,0},{255,0,0},{255,0,0}],
        [{0,255,0},{0,255,0},{0,255,0},{0,255,0},{0,255,0},{0,255,0},{0,255,0},{0,255,0}],
        [{0,0,255},{0,0,255},{0,0,255},{0,0,255},{0,0,255},{0,0,255},{0,0,255},{0,0,255}]
    ]).