-module(mandel).
-compile(export_all).

%
%   mandel
%

init() ->
    code:add_path("../helper"),
    code:add_path("../introduction").

mandelbrot(Width, Height, X, Y, K, Depth) ->
    Trans = fun(W, H) ->
        complex:new(X + K * (W - 1), Y - K * (H - 1))
    end,
    rows(Width, Height, Trans, Depth, []).

rows(_, 0, _, _, Rows) -> Rows;
rows(Width, Height, Trans, Depth, Rows) ->
    New = row(Width, Height, Trans, Depth, []),
    rows(Width, Height - 1, Trans, Depth, [New | Rows]).

row(0, _, _, _, Cols) -> Cols;
row(Width, Height, Trans, Depth, Cols) ->
    C = Trans(Width, Height),
    Color = brotcolor:convert(brot:mandelbrot(C, Depth), Depth),
    row(Width - 1, Height, Trans, Depth, [Color | Cols]).

%
%   demo
%

demo() ->
    init(),
    small(-2.6,1.2,1.6).

small(X,Y,X1) ->
    Width = 960,
    Height = 540,
    K = (X1 - X)/Width,
    Depth = 64,
    T0 = erlang:timestamp(),
    Image = mandelbrot(Width, Height, X, Y, K, Depth),
    T = timer:now_diff(erlang:timestamp(), T0),
    io:format("picture generated in ~w ms~n", [T div 1000]),
    ppm:write("small.jpg", Image).