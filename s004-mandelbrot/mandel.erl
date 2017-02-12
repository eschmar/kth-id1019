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
    Color = theme:convert(brot:mandelbrot(C, Depth), Depth),
    row(Width - 1, Height, Trans, Depth, [Color | Cols]).

%
%   demo
%

brot(Width, Height, X, Y, X1, Depth, FileName) ->
    init(),

    K = (X1 - X) / Width,
    T0 = erlang:timestamp(),
    Image = mandelbrot(Width, Height, X, Y, K, Depth),
    T = timer:now_diff(erlang:timestamp(), T0),
    color:out("Config:", cyan),
    color:out(io_lib:format("Z = {~w, ~w}", [X, Y]), cyan),
    color:out(io_lib:format("x1 = ~w", [X1]), cyan),
    color:out(io_lib:format("Delta = ~w", [K]), cyan),
    color:out(io_lib:format("Picture generated in ~w ms", [T div 1000]), magenta),
    color:out(io_lib:format("Written to ~s", [FileName]), green),
    Path = "img/" ++ FileName,
    ppm:write(Path, Image).

% whole mandelbrot set
whole() -> brot(960, 560, -2.6, 1.2, 1.6, 64, "mandelbrot.ppm").

% discover the mandelbrot set based on coordinates of the whole image
coord(X, Y, X1) when (X =< 960) and (Y =< 560) ->
    Real = (-2.6 + X * 0.004375),
    Imaginary = (1.2 - Y * 0.004375),
    FileName = io_lib:format("coord-~w-~w-~w.ppm", [X, Y, X1]),
    brot(960, 560, Real, Imaginary, X1, 256, FileName).

% pre given demo
demo() -> brot(1920, 1080, -0.14, 0.85, -0.13, 128, "demo.ppm").

% more demos
interesting() -> 
    brot(1920, 1080, -0.005624999999999769, 0.766875, 0.013, 256, "interesting.ppm").
