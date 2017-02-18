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
    % test(I, 0.0, 0.0, CR, CI, M).

test(M, _Zr, _Zi, _Cr, _Ci, M) ->
    0;

test(I, Zr, Zi, Cr, Ci, M) ->
    Zr2 = Zr * Zr,
    Zi2 = Zi * Zi,
    A2 = Zr2 + Zi2,
    if 
        A2 < 4.0 ->
            Sr = Zr2 - Zi2 + Cr,
            Si = 2*Zr*Zi + Ci,
            test(I+1, Sr, Si, Cr, Ci, M);
        true ->
            I
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